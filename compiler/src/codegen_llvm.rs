use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, BasicType}, // BasicType trait é necessário para .fn_type()
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
    IntPredicate, // Para comparações
};
use crate::ast::*; // Certifique-se que importa todos os nós da AST necessários
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
    variables: HashMap<String, (PointerValue<'ctx>, TypeAnnotationNode)>, // Armazena ponteiro e tipo AST
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

        // Declara as funções C intrínsecas no módulo LLVM.
        generator.declare_intrinsic_puts_fn();
        generator.declare_intrinsic_printf_fn();

        generator
    }

    fn get_llvm_type(&self, ty_node: &TypeAnnotationNode) -> Option<BasicTypeEnum<'ctx>> {
        match ty_node {
            TypeAnnotationNode::Number => Some(self.context.i32_type().into()),
            TypeAnnotationNode::String => Some(self.context.i8_type().ptr_type(AddressSpace::default()).into()),
            TypeAnnotationNode::Boolean => Some(self.context.bool_type().into()),
            TypeAnnotationNode::Void => None, // Void não é um BasicTypeEnum diretamente
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
            return_llvm_type.fn_type(&llvm_param_types, false) // Utiliza BasicType::fn_type
        } else {
            self.context.void_type().fn_type(&llvm_param_types, false)
        }
    }

    fn declare_intrinsic_puts_fn(&self) -> FunctionValue<'ctx> {
        let char_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        // puts(const char *s) -> int
        let puts_fn_type = self.context.i32_type().fn_type(&[char_ptr_type.into()], false);
        self.module.add_function("puts", puts_fn_type, Some(inkwell::module::Linkage::External))
    }

    fn declare_intrinsic_printf_fn(&self) -> FunctionValue<'ctx> {
        let char_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        // printf(const char *format, ...) -> int
        let printf_fn_type = self.context.i32_type().fn_type(&[char_ptr_type.into()], true); // true para varargs
        self.module.add_function("printf", printf_fn_type, Some(inkwell::module::Linkage::External))
    }

    fn create_entry_block_alloca(&self, name: &str, ty: BasicTypeEnum<'ctx>) -> PointerValue<'ctx> {
        let current_fn = self.builder.get_insert_block().and_then(|b| b.get_parent())
            .expect("CodeGenerator::create_entry_block_alloca: Builder is not positioned inside a function.");
        let temp_builder = self.context.create_builder();
        let entry_block = current_fn.get_first_basic_block()
            .expect("CodeGenerator::create_entry_block_alloca: Function does not have an entry block.");

        match entry_block.get_first_instruction() {
            Some(first_instr) => temp_builder.position_before(&first_instr),
            None => temp_builder.position_at_end(entry_block),
        }
        // Em versões recentes do Inkwell, build_alloca pode retornar Result
        temp_builder.build_alloca(ty, name)
            .unwrap_or_else(|e| panic!("Codegen Panic: Failed to build alloca for '{}' with type {:?}: {}", name, ty, e))
    }

    fn codegen_literal(&mut self, literal: &LiteralNode) -> CodegenResult<BasicValueEnum<'ctx>> {
        match literal {
            LiteralNode::Number(n) => Ok(self.context.i32_type().const_int(*n as u64, false).into()),
            LiteralNode::String(s_val) => {
                let string_ptr = self.builder.build_global_string_ptr(s_val, ".str_literal")
                    .map_err(|e| CodegenError::LlvmError(format!("Failed to build global string_ptr for '{}': {}", s_val, e)))?;
                Ok(string_ptr.as_pointer_value().into())
            },
            LiteralNode::Boolean(b) => Ok(self.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into()),
        }
    }

    fn codegen_function_call(&mut self, callee_node: &IdentifierNode, args_ast: &[ExpressionNode]) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let function_name_greenp = &callee_node.0;

        // Tratamento especial para funções "intrínsecas" da GreenP
        if function_name_greenp == "printString" {
            if args_ast.len() != 1 {
                return Err(CodegenError::ArgumentError("printString expects 1 argument".to_string()));
            }
            let arg_val_llvm = self.codegen_expression(&args_ast[0])?;
            if !arg_val_llvm.is_pointer_value() {
                 return Err(CodegenError::TypeMismatch("printString argument must be a string (pointer)".to_string()));
            }
            // Obtém a função "puts" do C (declarada anteriormente)
            let puts_fn_llvm = self.module.get_function("puts")
                .ok_or_else(|| CodegenError::UndefinedFunction("C function 'puts' (for printString) not found in LLVM module".to_string()))?;

            self.builder.build_call(puts_fn_llvm, &[arg_val_llvm.into()], "call_puts")
                .map_err(|e| CodegenError::LlvmError(format!("Failed to build call to puts: {}", e)))?;
            return Ok(None); // printString é void para GreenP

        } else if function_name_greenp == "printNumber" {
            if args_ast.len() != 1 {
                 return Err(CodegenError::ArgumentError("printNumber expects 1 argument".to_string()));
            }
            let arg_val_llvm = self.codegen_expression(&args_ast[0])?;
            if !arg_val_llvm.is_int_value() { // GreenP 'number' é i32 no MVP
                 return Err(CodegenError::TypeMismatch("printNumber argument must be a number (integer)".to_string()));
            }
            let printf_fn_llvm = self.module.get_function("printf")
                 .ok_or_else(|| CodegenError::UndefinedFunction("C function 'printf' (for printNumber) not found in LLVM module".to_string()))?;

            // Criar a string de formato para printf ("%d\n")
            let format_str_ptr = self.builder.build_global_string_ptr("%d\n", ".format_num_println").unwrap(); // unwrap ok para literal constante

            self.builder.build_call(printf_fn_llvm, &[format_str_ptr.as_pointer_value().into(), arg_val_llvm.into()], "call_printf")
                .map_err(|e| CodegenError::LlvmError(format!("Failed to build call to printf: {}", e)))?;
            return Ok(None); // printNumber é void para GreenP
        }

        // Funções definidas pelo usuário GreenP
        let function_llvm = self.function_value_map
            .get(function_name_greenp)
            .copied() // FunctionValue é Copy
            .ok_or_else(|| CodegenError::UndefinedFunction(format!("GreenP function '{}' not found in function map", function_name_greenp)))?;

        if function_llvm.count_params() as usize != args_ast.len() {
            return Err(CodegenError::ArgumentError(format!(
                "Function '{}' expects {} arguments, but {} were provided",
                function_name_greenp, function_llvm.count_params(), args_ast.len()
            )));
        }

        let mut compiled_args_llvm = Vec::new();
        for arg_expr_ast in args_ast {
            compiled_args_llvm.push(self.codegen_expression(arg_expr_ast)?.into());
            // TODO: Verificação de tipo dos argumentos contra os parâmetros da função LLVM (idealmente no semantic_analyzer)
        }

        let call_site_value = self.builder.build_call(function_llvm, &compiled_args_llvm, "tmpcall")
            .map_err(|e| CodegenError::LlvmError(format!("Failed to build call to {}: {}", function_name_greenp, e)))?;

        Ok(call_site_value.try_as_basic_value().left()) // Retorna Some(value) ou None se for void
    }

    fn codegen_expression(&mut self, expr_ast: &ExpressionNode) -> CodegenResult<BasicValueEnum<'ctx>> {
        match expr_ast {
            ExpressionNode::Literal(lit_node) => self.codegen_literal(lit_node),
            ExpressionNode::Identifier(id_node) => {
                let (var_ptr_llvm, var_type_ast_cloned) = self.variables.get(&id_node.0)
                    .cloned() // TypeAnnotationNode é Copy (e Clone), PointerValue é Copy
                    .ok_or_else(|| CodegenError::UndefinedVariable(format!("Variable '{}' not found", id_node.0)))?;

                let var_llvm_type = self.get_llvm_type(&var_type_ast_cloned)
                    .ok_or_else(|| CodegenError::TypeMismatch(format!("Cannot load variable '{}', its AST type {:?} is invalid or void", id_node.0, var_type_ast_cloned)))?;

                self.builder.build_load(var_llvm_type, var_ptr_llvm, &format!("load_{}", id_node.0))
                    .map_err(|e| CodegenError::LlvmError(format!("Failed to build load for var '{}': {}", id_node.0, e)))
            },
            ExpressionNode::BinaryOp { op, left, right } => {
                let lhs_llvm = self.codegen_expression(left)?;
                let rhs_llvm = self.codegen_expression(right)?;

                // Para MVP, focamos em operações i32 e booleanas para comparações
                // A análise semântica deve garantir tipos corretos
                match op {
                    BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
                        if !lhs_llvm.is_int_value() || !rhs_llvm.is_int_value() {
                            return Err(CodegenError::TypeMismatch(format!("Arithmetic operator {:?} requires integer operands", op)));
                        }
                        let lhs_int = lhs_llvm.into_int_value();
                        let rhs_int = rhs_llvm.into_int_value();
                        let result = match op {
                            BinaryOperator::Add => self.builder.build_int_add(lhs_int, rhs_int, "addtmp").unwrap(),
                            BinaryOperator::Sub => self.builder.build_int_sub(lhs_int, rhs_int, "subtmp").unwrap(),
                            BinaryOperator::Mul => self.builder.build_int_mul(lhs_int, rhs_int, "multmp").unwrap(),
                            BinaryOperator::Div => self.builder.build_int_signed_div(lhs_int, rhs_int, "divtmp").unwrap(),
                            BinaryOperator::Mod => self.builder.build_int_signed_rem(lhs_int, rhs_int, "modtmp").unwrap(),
                            _ => unreachable!(), // Outros ops são tratados abaixo ou acima
                        };
                        Ok(result.into())
                    }
                    BinaryOperator::Eq | BinaryOperator::Neq | BinaryOperator::StrictEq | BinaryOperator::StrictNeq |
                    BinaryOperator::Lt | BinaryOperator::Lte | BinaryOperator::Gt | BinaryOperator::Gte => {
                        // Assumindo i32 para comparações no MVP. String/bool comparison requereria mais lógica.
                        if !lhs_llvm.is_int_value() || !rhs_llvm.is_int_value() {
                             return Err(CodegenError::TypeMismatch(format!("Comparison operator {:?} currently requires integer operands", op)));
                        }
                        let lhs_int = lhs_llvm.into_int_value();
                        let rhs_int = rhs_llvm.into_int_value();
                        let llvm_predicate = match op {
                            BinaryOperator::Eq | BinaryOperator::StrictEq => IntPredicate::EQ,
                            BinaryOperator::Neq | BinaryOperator::StrictNeq => IntPredicate::NE,
                            BinaryOperator::Lt => IntPredicate::SLT, // Signed Less Than
                            BinaryOperator::Lte => IntPredicate::SLE, // Signed Less Than or Equal
                            BinaryOperator::Gt => IntPredicate::SGT,  // Signed Greater Than
                            BinaryOperator::Gte => IntPredicate::SGE, // Signed Greater Than or Equal
                            _ => unreachable!(),
                        };
                        let result = self.builder.build_int_compare(llvm_predicate, lhs_int, rhs_int, "cmptmp").unwrap();
                        Ok(result.into())
                    }
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                        // Requer short-circuiting (blocos básicos), não uma instrução LLVM simples para inteiros/bools
                        // Para MVP, isto é um erro ou uma simplificação que não faz short-circuit
                        if !lhs_llvm.is_int_value() || !rhs_llvm.is_int_value() { // LLVM and/or são bitwise em inteiros
                             return Err(CodegenError::TypeMismatch(format!("Logical operator {:?} currently requires boolean (i1) operands, represented as integers", op)));
                        }
                        let lhs_bool = lhs_llvm.into_int_value(); // Espera-se i1 do analisador semântico
                        let rhs_bool = rhs_llvm.into_int_value(); // Espera-se i1
                        let result = match op {
                             BinaryOperator::LogicalAnd => self.builder.build_and(lhs_bool, rhs_bool, "andtmp").unwrap(),
                             BinaryOperator::LogicalOr => self.builder.build_or(lhs_bool, rhs_bool, "ortmp").unwrap(),
                             _ => unreachable!(),
                        };
                        Ok(result.into())
                    }
                }
            },
            ExpressionNode::FunctionCall { callee, args } => {
                self.codegen_function_call(callee, args)? // Retorna Ok(Option<BasicValueEnum>)
                    .ok_or_else(|| CodegenError::TypeMismatch("Function call used as expression must return a value (not void)".to_string()))
            },
        }
    }

    fn codegen_statement(&mut self, stmt_ast: &Statement) -> CodegenResult<()> {
        match stmt_ast {
            Statement::FunctionDeclaration(func_decl_node) => self.codegen_function_declaration(func_decl_node),
            Statement::VariableDeclaration { is_const: _, name, ty_annotation, initializer } => {
                let var_name = &name.0;
                let type_node_cloned = ty_annotation.as_ref().ok_or_else(||
                    CodegenError::TypeMismatch(format!("Variable '{}' is missing a type annotation for codegen MVP", var_name))
                )?.clone(); // Clonado para evitar problemas de borrow checker

                let llvm_var_type = self.get_llvm_type(&type_node_cloned)
                    .ok_or_else(|| CodegenError::TypeMismatch(format!("Type for variable '{}' ({:?}) is invalid or resolves to void", var_name, type_node_cloned)))?;

                let var_alloca_ptr = self.create_entry_block_alloca(var_name, llvm_var_type);

                if let Some(init_expr_ast) = initializer {
                    let init_val_llvm = self.codegen_expression(init_expr_ast)?;
                    // A análise semântica já deveria ter verificado isso
                    if init_val_llvm.get_type() != llvm_var_type {
                        return Err(CodegenError::TypeMismatch(
                            format!("Initializer type for '{}' ({:?}) does not match variable type declaration ({:?}). LLVM types: {:?} vs {:?}",
                                    var_name, self.get_llvm_type(&self.analyze_expr_type_for_error(init_expr_ast)?).unwrap_or(self.context.i8_type().into()), type_node_cloned,
                                    init_val_llvm.get_type(), llvm_var_type )));
                    }
                    self.builder.build_store(var_alloca_ptr, init_val_llvm)
                        .map_err(|e| CodegenError::LlvmError(format!("Failed to build store for var '{}': {}", var_name, e)))?;
                }
                self.variables.insert(var_name.clone(), (var_alloca_ptr, type_node_cloned));
                Ok(())
            },
            Statement::ExpressionStatement(expr_ast) => {
                self.codegen_expression(expr_ast)?; // O valor é descartado
                Ok(())
            },
            Statement::ReturnStatement(expr_opt_ast) => {
                // Clonar para evitar problemas de borrow com self.current_function_return_type
                let expected_return_type_ast_cloned = self.current_function_return_type.as_ref()
                    .ok_or(CodegenError::InvalidOperation("Return statement outside of a function body".to_string()))?
                    .clone();

                if let Some(return_expr_ast) = expr_opt_ast {
                    if matches!(expected_return_type_ast_cloned, TypeAnnotationNode::Void) {
                        return Err(CodegenError::TypeMismatch("Cannot return a value from a function declared as void".to_string()));
                    }
                    // Mutable borrow of self:
                    let value_to_return_llvm = self.codegen_expression(return_expr_ast)?;

                    // Use a AST clonada para obter o tipo LLVM esperado
                    let expected_llvm_return_type = self.get_llvm_type(&expected_return_type_ast_cloned)
                        .ok_or_else(|| CodegenError::TypeMismatch(format!("Declared function return type {:?} is invalid or resolves to void unexpectedly", expected_return_type_ast_cloned)))?;

                    // A análise semântica já deveria ter verificado isso
                    if value_to_return_llvm.get_type() != expected_llvm_return_type {
                         return Err(CodegenError::TypeMismatch(
                            format!("Return value's LLVM type ({:?}) does not match function's expected LLVM return type ({:?}). AST types: expected {:?}, got {:?}",
                                    value_to_return_llvm.get_type(), expected_llvm_return_type,
                                    expected_return_type_ast_cloned, self.analyze_expr_type_for_error(return_expr_ast)? )));
                    }
                    self.builder.build_return(Some(&value_to_return_llvm))
                        .map_err(|e| CodegenError::LlvmError(format!("Failed to build return with value: {}", e)))?;
                } else { // return;
                    if !matches!(expected_return_type_ast_cloned, TypeAnnotationNode::Void) {
                        return Err(CodegenError::TypeMismatch(format!("Function expects a return value of type {:?}, but found an empty return.", expected_return_type_ast_cloned)));
                    }
                    self.builder.build_return(None)
                        .map_err(|e| CodegenError::LlvmError(format!("Failed to build void return: {}", e)))?;
                }
                Ok(())
            },
            Statement::Empty => Ok(()), // Empty statements fazem nada
            Statement::Block(block_node) => {
                // Para MVP, blocos standalone não criam novo escopo de variáveis no codegen ainda.
                // Isso exigiria empilhar/desempilhar o `self.variables`.
                // A análise semântica DEVE lidar com escopos corretamente.
                for stmt in &block_node.statements {
                    self.codegen_statement(stmt)?;
                     if self.builder.get_insert_block().and_then(|b| b.get_terminator()).is_some() {
                        // Se um return foi encontrado dentro do bloco, não continuar.
                        break;
                    }
                }
                Ok(())
            }
            // Stubs para funcionalidades futuras
            Statement::IfStatement { .. } => Err(CodegenError::NotImplemented("IfStatement codegen".to_string())),
            Statement::EnergyBlock { .. } => Err(CodegenError::NotImplemented("EnergyBlock codegen".to_string())),
            Statement::ResourceAcquisition { .. } => Err(CodegenError::NotImplemented("ResourceAcquisition codegen".to_string())),
        }
    }
    
    // Função helper para mensagens de erro (simulada, pois não temos análise de tipo aqui)
    fn analyze_expr_type_for_error(&self, _expr_ast: &ExpressionNode) -> CodegenResult<TypeAnnotationNode> {
        // Esta é uma simplificação. Em um codegen real, você confiaria no output do semantic analyzer.
        // Para fins de mensagem de erro, poderíamos tentar uma inferência muito básica ou retornar um tipo desconhecido.
        Ok(TypeAnnotationNode::Number) // Placeholder
    }


    fn codegen_function_declaration(&mut self, func_decl_node: &FunctionDeclarationNode) -> CodegenResult<()> {
        let param_types_ast: Vec<TypeAnnotationNode> = func_decl_node.params.iter()
            .map(|param| param.ty_annotation) // .clone() não é necessário se TypeAnnotationNode é Copy
            .collect();

        let fn_llvm_type = self.get_llvm_function_type(&param_types_ast, &func_decl_node.return_type);
        let function_llvm_value = self.module.add_function(&func_decl_node.name.0, fn_llvm_type, None);

        self.function_value_map.insert(func_decl_node.name.0.clone(), function_llvm_value);

        let entry_block = self.context.append_basic_block(function_llvm_value, "entry");
        self.builder.position_at_end(entry_block);

        // Salvar e limpar escopo de variáveis para a nova função
        let previous_variables_scope = std::mem::take(&mut self.variables);
        let previous_return_type = self.current_function_return_type.take();
        self.current_function_return_type = Some(func_decl_node.return_type); // .clone() não é necessário se Copy

        // Processar parâmetros: alocar na pilha e armazenar os valores dos parâmetros LLVM
        for (param_ast, llvm_param_value) in func_decl_node.params.iter().zip(function_llvm_value.get_param_iter()) {
            let param_name = &param_ast.name.0;
            let param_ast_type_node = param_ast.ty_annotation; // Já é TypeAnnotationNode, Copy
            let param_llvm_type = self.get_llvm_type(&param_ast_type_node)
                .ok_or_else(|| CodegenError::TypeMismatch(format!("Parameter '{}' type annotation ({:?}) is invalid or resolves to void", param_name, param_ast_type_node)))?;

            llvm_param_value.set_name(param_name); // Nomeia o parâmetro LLVM

            let param_alloca_ptr = self.create_entry_block_alloca(param_name, param_llvm_type);
            self.builder.build_store(param_alloca_ptr, llvm_param_value)
                .map_err(|e| CodegenError::LlvmError(format!("Failed to build store for param '{}': {}", param_name, e)))?;
            self.variables.insert(param_name.clone(), (param_alloca_ptr, param_ast_type_node));
        }

        // Gerar corpo da função
        let mut has_terminator_in_body = false;
        if func_decl_node.body.statements.is_empty() {
             if self.builder.get_insert_block().and_then(|b| b.get_terminator()).is_none() {
                // Bloco de entrada está vazio e não tem terminador.
                // A lógica !has_terminator_in_body abaixo cuidará disso.
             } else {
                has_terminator_in_body = true; // Bloco de entrada já tinha um terminador (improvável para função vazia)
             }
        } else {
            for statement_ast in &func_decl_node.body.statements {
                self.codegen_statement(statement_ast)?;
                // Se o statement atual (ex: return) adicionou um terminador ao bloco atual
                if self.builder.get_insert_block().and_then(|b| b.get_terminator()).is_some() {
                    has_terminator_in_body = true;
                    break; // Não gerar código após um terminador no mesmo bloco
                }
            }
        }

        // Adicionar return implícito se necessário (para funções void ou se o último statement não é return)
        if !has_terminator_in_body {
            // Verifica se o bloco atual (onde o builder está posicionado) tem um terminador.
            // Isso é importante porque um statement de bloco interno pode não ter adicionado um terminador
            // ao bloco principal da função.
            if let Some(current_block) = self.builder.get_insert_block() {
                 if current_block.get_terminator().is_none() { // Só adiciona return se o bloco atual não tem
                    if matches!(func_decl_node.return_type, TypeAnnotationNode::Void) {
                        self.builder.build_return(None)
                            .map_err(|e| CodegenError::LlvmError(format!("Failed to build implicit void return for func '{}': {}", func_decl_node.name.0, e)))?;
                    } else {
                        // Se a função não é void e todos os caminhos não retornam, é um erro.
                        // A análise semântica deveria pegar isso, mas uma verificação aqui é uma salvaguarda.
                        return Err(CodegenError::InvalidOperation(format!(
                            "Function '{}' has non-void return type {:?} but lacks a return statement on some execution paths.",
                            func_decl_node.name.0, func_decl_node.return_type
                        )));
                    }
                }
            } else {
                 // Se o builder não está posicionado, significa que o último bloco não é conhecido, o que é um estado inválido
                 // se a função não é void. Para funções void, pode ser aceitável se todos os caminhos terminam.
                 if !matches!(func_decl_node.return_type, TypeAnnotationNode::Void) {
                      return Err(CodegenError::InvalidOperation(format!(
                        "Function '{}' ({:?}) codegen ended with builder unpositioned, and no return was found for non-void function.",
                        func_decl_node.name.0, func_decl_node.return_type
                    )));
                 }
            }
        }

        // Restaurar escopo de variáveis e tipo de retorno
        self.variables = previous_variables_scope;
        self.current_function_return_type = previous_return_type;

        // Verificar função (opcionalmente mover para após todas as funções serem geradas)
        if function_llvm_value.verify(true) {
            Ok(())
        } else {
            // Descomente para imprimir o IR inválido para stderr:
            // function_llvm_value.print_to_stderr();
            Err(CodegenError::LlvmError(format!("Invalid LLVM function generated for '{}'. IR verification failed.", func_decl_node.name.0)))
        }
    }

    pub fn codegen_program(&mut self, program_node: &ProgramNode) -> CodegenResult<()> {
        for statement_ast in &program_node.body {
            match statement_ast {
                Statement::FunctionDeclaration(_) => self.codegen_statement(statement_ast)?,
                // Outros tipos de statements de nível superior (imports, etc.) não são MVP
                _ => return Err(CodegenError::InvalidOperation(
                    "Only function declarations are allowed at the top level of a GreenP program in MVP".to_string()
                )),
            }
        }
        Ok(())
    }

    pub fn into_module(self) -> Module<'ctx> {
        self.module
    }
}