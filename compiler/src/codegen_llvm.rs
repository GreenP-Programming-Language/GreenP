use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, BasicType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use crate::ast::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("LLVM error: {0}")]
    LlvmError(String),
    #[error("Type mismatch: {0}")]
    TypeMismatch(String),
    #[error("Undefined function: {0}")]
    UndefinedFunction(String),
    #[error("Undefined variable: {0}")] 
    UndefinedVariable(String),
    #[error("Invalid argument: {0}")]
    ArgumentError(String),
    #[error("Not implemented: {0}")]
    NotImplemented(String),
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
}

pub type CodegenResult<T> = Result<T, CodegenError>;

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>, 
    function_value_map: HashMap<String, FunctionValue<'ctx>>,
    variables: HashMap<String, (PointerValue<'ctx>, TypeAnnotationNode)>, 
    current_function_return_type: Option<TypeAnnotationNode>, 
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let generator = CodeGenerator {
            context,
            module,
            builder,
            function_value_map: HashMap::new(),
            variables: HashMap::new(),
            current_function_return_type: None,
        };
        
        // Apenas declara as funções C no módulo. O mapeamento de `printString` para `puts`
        // é feito em `codegen_function_call`.
        generator.declare_intrinsic_puts_fn(); 
        generator.declare_intrinsic_printf_fn();

        generator
    }

    fn get_llvm_type(&self, ty_node: &TypeAnnotationNode) -> Option<BasicTypeEnum<'ctx>> {
        match ty_node {
            TypeAnnotationNode::Number => Some(self.context.i32_type().into()),
            TypeAnnotationNode::String => Some(self.context.i8_type().ptr_type(AddressSpace::default()).into()),
            TypeAnnotationNode::Boolean => Some(self.context.bool_type().into()),
            TypeAnnotationNode::Void => None,
        }
    }

    fn get_llvm_function_type(&self, param_types_ast: &[TypeAnnotationNode], return_type_ast: &TypeAnnotationNode) 
        -> inkwell::types::FunctionType<'ctx> 
    {
        let llvm_param_types: Vec<BasicMetadataTypeEnum<'ctx>> = param_types_ast
            .iter()
            .filter_map(|ty_node| self.get_llvm_type(ty_node).map(|bt| bt.into()))
            .collect();

        if let Some(return_llvm_type) = self.get_llvm_type(return_type_ast) {
            return_llvm_type.fn_type(&llvm_param_types, false)
        } else {
            self.context.void_type().fn_type(&llvm_param_types, false)
        }
    }

    fn declare_intrinsic_puts_fn(&self) -> FunctionValue<'ctx> {
        let c_char_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let puts_fn_type = self.context.i32_type().fn_type(&[c_char_ptr_type.into()], false); 
        self.module.add_function("puts", puts_fn_type, Some(inkwell::module::Linkage::External))
    }

    fn declare_intrinsic_printf_fn(&self) -> FunctionValue<'ctx> {
        let c_char_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let printf_fn_type = self.context.i32_type().fn_type(&[c_char_ptr_type.into()], true); 
        self.module.add_function("printf", printf_fn_type, Some(inkwell::module::Linkage::External))
    }
    
    fn create_entry_block_alloca(&self, name: &str, ty: BasicTypeEnum<'ctx>) -> PointerValue<'ctx> {
        let current_fn = self.builder.get_insert_block().and_then(|b| b.get_parent())
            .expect("Builder is not positioned inside a function for alloca");
        let temp_builder = self.context.create_builder();
        let entry_block = current_fn.get_first_basic_block()
            .expect("Function does not have an entry block for alloca");

        match entry_block.get_first_instruction() {
            Some(first_instr) => temp_builder.position_before(&first_instr),
            None => temp_builder.position_at_end(entry_block),
        }
        temp_builder.build_alloca(ty, name)
            .unwrap_or_else(|e| panic!("Failed to build alloca for {} with type {:?}: {}", name, ty, e))
    }

    fn codegen_literal(&mut self, literal: &LiteralNode) -> CodegenResult<BasicValueEnum<'ctx>> {
        match literal {
            LiteralNode::Number(n) => Ok(self.context.i32_type().const_int(*n as u64, false).into()),
            LiteralNode::String(s_val) => {
                let string_ptr = self.builder.build_global_string_ptr(s_val, ".str_literal")
                    .map_err(|e| CodegenError::LlvmError(format!("CodegenError: Failed to build global string_ptr for '{}': {}", s_val, e)))?;
                Ok(string_ptr.as_pointer_value().into())
            },
            LiteralNode::Boolean(b) => Ok(self.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into()),
        }
    }

    fn codegen_function_call(&mut self, callee_node: &IdentifierNode, args: &[ExpressionNode]) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let function_name_ast = &callee_node.0;

        if function_name_ast == "printString" {
            if args.len() != 1 {
                return Err(CodegenError::ArgumentError("printString expects 1 argument".to_string()));
            }
            let arg_value = self.codegen_expression(&args[0])?;
            if !arg_value.is_pointer_value() { 
                 return Err(CodegenError::TypeMismatch("printString argument must be a string (pointer)".to_string()));
            }
            let puts_fn = self.module.get_function("puts")
                .ok_or_else(|| CodegenError::UndefinedFunction("C function 'puts' for printString not declared".to_string()))?;
            
            self.builder.build_call(puts_fn, &[arg_value.into()], "call_puts")
                .map_err(|e| CodegenError::LlvmError(format!("Failed to build call to puts: {}", e)))?;
            return Ok(None);

        } else if function_name_ast == "printNumber" {
            if args.len() != 1 {
                 return Err(CodegenError::ArgumentError("printNumber expects 1 value argument".to_string()));
            }
            let value_to_print = self.codegen_expression(&args[0])?;
            if !value_to_print.is_int_value() {
                 return Err(CodegenError::TypeMismatch("printNumber argument must be a number (integer)".to_string()));
            }
            let printf_fn = self.module.get_function("printf")
                 .ok_or_else(|| CodegenError::UndefinedFunction("C function 'printf' for printNumber not declared".to_string()))?;

            let format_str_ptr = self.builder.build_global_string_ptr("%d\n", ".format_num_println").unwrap();
            
            self.builder.build_call(printf_fn, &[format_str_ptr.as_pointer_value().into(), value_to_print.into()], "call_printf")
                .map_err(|e| CodegenError::LlvmError(format!("Failed to build call to printf: {}", e)))?;
            return Ok(None);
        }

        let function_llvm = self.function_value_map
            .get(function_name_ast)
            .copied()
            .ok_or_else(|| CodegenError::UndefinedFunction(function_name_ast.clone()))?;

        let mut compiled_args = Vec::new();
        if function_llvm.count_params() as usize != args.len() {
            return Err(CodegenError::ArgumentError(format!(
                "Function '{}' expects {} arguments, but {} were provided",
                function_name_ast, function_llvm.count_params(), args.len()
            )));
        }
        for arg_expr in args {
            compiled_args.push(self.codegen_expression(arg_expr)?.into());
        }
        
        let call_site_value = self.builder.build_call(function_llvm, &compiled_args, "tmpcall")
            .map_err(|e| CodegenError::LlvmError(format!("Failed to build call to {}: {}", function_name_ast, e)))?;

        Ok(call_site_value.try_as_basic_value().left())
    }

    fn codegen_expression(&mut self, expr: &ExpressionNode) -> CodegenResult<BasicValueEnum<'ctx>> {
        match expr {
            ExpressionNode::Literal(lit) => self.codegen_literal(lit),
            ExpressionNode::Identifier(id) => {
                let (ptr_val, var_type_node) = self.variables.get(&id.0)
                    .cloned() // <<< CHANGE .copied() to .cloned()
                    .ok_or_else(|| CodegenError::UndefinedVariable(format!("Variable '{}' not found", id.0)))?;
                
                let llvm_var_type = self.get_llvm_type(&var_type_node) // var_type_node is now owned TypeAnnotationNode
                    .ok_or_else(|| CodegenError::TypeMismatch(format!("Cannot load variable '{}' of type void or unknown type {:?}", id.0, var_type_node)))?;

                self.builder.build_load(llvm_var_type, ptr_val, &format!("load_{}", id.0))
                    .map_err(|e| CodegenError::LlvmError(format!("CodegenError: Failed to build load for var '{}': {}", id.0, e)))
            },
            ExpressionNode::BinaryOp { op, left, right } => {
                let lhs = self.codegen_expression(left)?;
                let rhs = self.codegen_expression(right)?;

                if !lhs.is_int_value() || !rhs.is_int_value() {
                    return Err(CodegenError::TypeMismatch("Binary operations currently only support integers".to_string()));
                }

                let lhs_int = lhs.into_int_value();
                let rhs_int = rhs.into_int_value();

                let result_op = match op {
                    BinaryOperator::Add => self.builder.build_int_add(lhs_int, rhs_int, "addtmp").unwrap(),
                    BinaryOperator::Sub => self.builder.build_int_sub(lhs_int, rhs_int, "subtmp").unwrap(),
                    BinaryOperator::Mul => self.builder.build_int_mul(lhs_int, rhs_int, "multmp").unwrap(),
                    BinaryOperator::Div => self.builder.build_int_signed_div(lhs_int, rhs_int, "divtmp").unwrap(),
                    BinaryOperator::Mod => self.builder.build_int_signed_rem(lhs_int, rhs_int, "modtmp").unwrap(),
                    BinaryOperator::Eq | BinaryOperator::StrictEq => 
                        self.builder.build_int_compare(inkwell::IntPredicate::EQ, lhs_int, rhs_int, "eqtmp").unwrap(),
                    BinaryOperator::Neq | BinaryOperator::StrictNeq =>
                        self.builder.build_int_compare(inkwell::IntPredicate::NE, lhs_int, rhs_int, "neqtmp").unwrap(),
                    BinaryOperator::Lt =>
                        self.builder.build_int_compare(inkwell::IntPredicate::SLT, lhs_int, rhs_int, "lttmp").unwrap(),
                    BinaryOperator::Lte =>
                        self.builder.build_int_compare(inkwell::IntPredicate::SLE, lhs_int, rhs_int, "ltetmp").unwrap(),
                    BinaryOperator::Gt =>
                        self.builder.build_int_compare(inkwell::IntPredicate::SGT, lhs_int, rhs_int, "gttmp").unwrap(),
                    BinaryOperator::Gte =>
                        self.builder.build_int_compare(inkwell::IntPredicate::SGE, lhs_int, rhs_int, "gtetmp").unwrap(),
                    _ => return Err(CodegenError::NotImplemented(format!("Binary operator {:?} not implemented yet for integers", op))),
                };
                Ok(result_op.into())
            },
            ExpressionNode::FunctionCall { callee, args } => {
                self.codegen_function_call(callee, args)?
                    .ok_or_else(|| CodegenError::TypeMismatch("Function call used as expression must return a value (not void)".to_string()))
            },
        }
    }

    fn codegen_statement(&mut self, stmt: &Statement) -> CodegenResult<()> {
        match stmt {
            Statement::FunctionDeclaration(func_decl_node) => self.codegen_function_declaration(func_decl_node),
            Statement::VariableDeclaration { is_const: _, name, ty_annotation, initializer } => {
                let var_name = &name.0;
                // Clone the type_node early to avoid borrow checker issues
                let type_node_cloned = ty_annotation.as_ref().ok_or_else(|| 
                    CodegenError::TypeMismatch(format!("Variable '{}' is missing a type annotation for codegen", var_name))
                )?.clone(); // <<< CLONE HERE

                let llvm_type = self.get_llvm_type(&type_node_cloned) // <<< USE CLONED VERSION
                    .ok_or_else(|| CodegenError::TypeMismatch(format!("Type for variable '{}' ({:?}) resolves to void or is invalid", var_name, type_node_cloned)))?;

                let alloca = self.create_entry_block_alloca(var_name, llvm_type);

                if let Some(init_expr) = initializer {
                    let init_val = self.codegen_expression(init_expr)?; // <<< Mutable borrow of self happens here
                    if init_val.get_type() != llvm_type {
                        return Err(CodegenError::TypeMismatch(
                            format!("Initializer type for '{}' does not match variable type. Expected {:?}, got {:?}", 
                                    var_name, llvm_type, init_val.get_type())));
                    }
                    self.builder.build_store(alloca, init_val)
                        .map_err(|e| CodegenError::LlvmError(format!("CodegenError: Failed to build store for var '{}': {}", var_name, e)))?;
                }
                // Use the cloned type_node here
                self.variables.insert(var_name.clone(), (alloca, type_node_cloned)); // <<< USE CLONED VERSION
                Ok(())
            },
            Statement::ExpressionStatement(expr) => {
                self.codegen_expression(expr)?;
                Ok(())
            },
            Statement::ReturnStatement(expr_opt) => {
                // Clone the AST representation of the return type *before* any mutable borrows of `self`.
                let return_type_ast_cloned = self.current_function_return_type.as_ref()
                    .ok_or(CodegenError::InvalidOperation("Return statement outside of a function body".to_string()))?
                    .clone(); // <<< CLONE HERE

                if let Some(return_expr) = expr_opt {
                    if matches!(return_type_ast_cloned, TypeAnnotationNode::Void) { // Use cloned version
                        return Err(CodegenError::TypeMismatch("Cannot return a value from a function declared as void".to_string()));
                    }
                    
                    // Mutable borrow of self occurs here:
                    let value_to_return = self.codegen_expression(return_expr)?; 
                    
                    // Now use the cloned AST type for checks and getting LLVM type
                    let expected_llvm_return_type = self.get_llvm_type(&return_type_ast_cloned) // Use cloned version
                        .ok_or_else(|| CodegenError::TypeMismatch(format!("Return type {:?} is invalid or resolves to void unexpectedly", return_type_ast_cloned)))?;
                                        
                    if value_to_return.get_type() != expected_llvm_return_type {
                         return Err(CodegenError::TypeMismatch(
                            format!("Return value type mismatch. Function expects {:?}, expression returned {:?}", 
                                    return_type_ast_cloned, value_to_return.get_type()))); // Improved error message
                    }
                    self.builder.build_return(Some(&value_to_return))
                        .map_err(|e| CodegenError::LlvmError(format!("CodegenError: Failed to build return with value: {}", e)))?;
                } else { // This is a `return;` statement
                    if !matches!(return_type_ast_cloned, TypeAnnotationNode::Void) { // Use cloned version
                        return Err(CodegenError::TypeMismatch(format!("Function expects a return value of type {:?}, but found an empty return.", return_type_ast_cloned)));
                    }
                    self.builder.build_return(None)
                        .map_err(|e| CodegenError::LlvmError(format!("CodegenError: Failed to build void return: {}", e)))?;
                }
                Ok(())
            },
            Statement::EnergyBlock { .. } => Err(CodegenError::NotImplemented("EnergyBlock codegen".to_string())),
            Statement::ResourceAcquisition { .. } => Err(CodegenError::NotImplemented("ResourceAcquisition codegen".to_string())),
            Statement::IfStatement { .. } => Err(CodegenError::NotImplemented("IfStatement codegen".to_string())),
            Statement::Block { .. } => Err(CodegenError::NotImplemented("Block statement codegen (standalone block)".to_string())),
        }
    }

    fn codegen_function_declaration(&mut self, func_decl_node: &FunctionDeclarationNode) -> CodegenResult<()> {
        let param_types_ast: Vec<TypeAnnotationNode> = func_decl_node.params.iter()
            .map(|param| param.ty_annotation.clone())
            .collect();

        let fn_llvm_type = self.get_llvm_function_type(&param_types_ast, &func_decl_node.return_type);
        let function_llvm_value = self.module.add_function(&func_decl_node.name.0, fn_llvm_type, None);

        self.function_value_map.insert(func_decl_node.name.0.clone(), function_llvm_value);

        let entry_block = self.context.append_basic_block(function_llvm_value, "entry");
        self.builder.position_at_end(entry_block);

        let previous_variables_scope = std::mem::take(&mut self.variables);
        let previous_return_type = self.current_function_return_type.take();
        self.current_function_return_type = Some(func_decl_node.return_type.clone());

        for (param_ast, llvm_param_value) in func_decl_node.params.iter().zip(function_llvm_value.get_param_iter()) {
            let param_name = &param_ast.name.0;
            let param_ast_type_node = &param_ast.ty_annotation;
            let param_llvm_type = self.get_llvm_type(param_ast_type_node)
                .ok_or_else(|| CodegenError::TypeMismatch(format!("Parameter '{}' type annotation ({:?}) resolves to void or is invalid", param_name, param_ast_type_node)))?;
            
            llvm_param_value.set_name(param_name);

            let alloca = self.create_entry_block_alloca(param_name, param_llvm_type);
            self.builder.build_store(alloca, llvm_param_value)
                .map_err(|e| CodegenError::LlvmError(format!("CodegenError: Failed to build store for param '{}': {}", param_name, e)))?;
            self.variables.insert(param_name.clone(), (alloca, param_ast_type_node.clone()));
        }

        let mut has_terminator = false;
        // Check if body is empty first, as get_terminator might panic on an empty unsealed block.
        if func_decl_node.body.statements.is_empty() && 
           !self.builder.get_insert_block().and_then(|b| b.get_terminator()).is_some() {
            // Empty body, handle terminator below
        } else {
            for statement_node in &func_decl_node.body.statements { 
                self.codegen_statement(statement_node)?; 
                if self.builder.get_insert_block().and_then(|b| b.get_terminator()).is_some() {
                    has_terminator = true;
                    break; 
                }
            }
        }
        
        if !has_terminator {
            if let Some(current_block) = self.builder.get_insert_block() {
                 if current_block.get_terminator().is_none() { // Check again if we are in a valid block
                    if matches!(func_decl_node.return_type, TypeAnnotationNode::Void) {
                        self.builder.build_return(None)
                            .map_err(|e| CodegenError::LlvmError(format!("CodegenError: Failed to build implicit void return for func '{}': {}", func_decl_node.name.0, e)))?;
                    } else {
                        return Err(CodegenError::InvalidOperation(format!(
                            "Function '{}' has non-void return type {:?} but lacks a return statement on some execution paths.",
                            func_decl_node.name.0, func_decl_node.return_type
                        )));
                    }
                }
            } else {
                 // Builder is not positioned, which means the last statement might have created a new block and not returned to it.
                 // This can happen if a statement like If/Else doesn't correctly manage builder position.
                 // For MVP, this might indicate an unhandled control flow scenario.
                 if !matches!(func_decl_node.return_type, TypeAnnotationNode::Void) {
                      return Err(CodegenError::InvalidOperation(format!(
                        "Function '{}' ({:?}) ended without the builder being positioned in a block, and no return was found.",
                        func_decl_node.name.0, func_decl_node.return_type
                    )));
                 }
            }
        }

        self.variables = previous_variables_scope;
        self.current_function_return_type = previous_return_type;

        if function_llvm_value.verify(true) {
            Ok(())
        } else {
            // function_llvm_value.print_to_stderr(); 
            Err(CodegenError::LlvmError(format!("Invalid LLVM function generated for '{}'. IR verification failed.", func_decl_node.name.0)))
        }
    }

    pub fn codegen_program(&mut self, program: &ProgramNode) -> CodegenResult<()> {
        for statement_node in &program.body { 
            match statement_node {
                Statement::FunctionDeclaration(_) => self.codegen_statement(statement_node)?, 
                _ => return Err(CodegenError::InvalidOperation(
                    "Only function declarations are allowed at the top level of a program in MVP".to_string()
                )),
            }
        }
        Ok(())
    }

    pub fn into_module(self) -> Module<'ctx> {
        self.module
    }
}