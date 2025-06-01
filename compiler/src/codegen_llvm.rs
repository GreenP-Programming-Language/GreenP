
impl<'ctx> CodeGenerator<'ctx> {
    // ... (new, etc.)

    fn get_llvm_type(&self, ty_node: &TypeAnnotationNode) -> Option<BasicTypeEnum<'ctx>> {
        match ty_node {
            TypeAnnotationNode::Number => Some(self.context.i32_type().into()), // MVP: number é i32
            TypeAnnotationNode::String => Some(self.context.i8_type().ptr_type(AddressSpace::default()).into()),
            TypeAnnotationNode::Boolean => Some(self.context.bool_type().into()),
            TypeAnnotationNode::Void => None, // Void não é um BasicTypeEnum diretamente
        }
    }

    fn get_llvm_fn_type(&self, param_types_ast: &[TypeAnnotationNode], return_type_ast: &TypeAnnotationNode) 
        -> inkwell::types::FunctionType<'ctx> 
    {
        let llvm_param_types: Vec<BasicMetadataTypeEnum<'ctx>> = param_types_ast
            .iter()
            .filter_map(|t| self.get_llvm_type(t).map(|bt| bt.into()))
            .collect();

        if let Some(ret_type_llvm) = self.get_llvm_type(return_type_ast) {
            ret_type_llvm.fn_type(&llvm_param_types, false)
        } else { // Assumindo Void
            self.context.void_type().fn_type(&llvm_param_types, false)
        }
    }

    // Declaração para printString (usando puts)
    fn declare_print_string_fn(&self) -> FunctionValue<'ctx> {
        if let Some(func) = self.module.get_function("puts") { // Usaremos puts para printString
            return func;
        }
        let i32_type = self.context.i32_type(); // puts retorna int
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let fn_type = i32_type.fn_type(&[i8_ptr_type.into()], false);
        self.module.add_function("puts", fn_type, Some(inkwell::module::Linkage::External))
    }

    // Declaração para printNumber (usando printf)
    fn declare_print_number_fn(&self) -> FunctionValue<'ctx> {
        if let Some(func) = self.module.get_function("printf") { // Usaremos printf
            return func;
        }
        let i32_type = self.context.i32_type(); // printf retorna int
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        // printf(format_string: char*, value: i32)
        let fn_type = i32_type.fn_type(&[i8_ptr_type.into()], true); // true para varargs
        self.module.add_function("printf", fn_type, Some(inkwell::module::Linkage::External))
    }
    
    // ... (codegen_expr ajustado para novos literais e ops)

    fn codegen_literal(&mut self, literal: &LiteralNode) -> CodegenResult<BasicValueEnum<'ctx>> {
        match literal {
            LiteralNode::Number(n) => Ok(self.context.i32_type().const_int(*n as u64, false).into()),
            LiteralNode::String(s_val) => {
                let c_str = self.builder.build_global_string_ptr(s_val.as_str(), ".str_literal")
                    .map_err(|e| CodegenError::LlvmError(format!("Failed to build global string: {}", e)))?;
                Ok(c_str.as_pointer_value().into())
            }
            LiteralNode::Boolean(b) => Ok(self.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into()),
        }
    }
    
     fn codegen_function_call(&mut self, callee: &IdentifierNode, args: &[ExpressionNode]) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let function_name = &callee.0;
        
        // Tratamento especial para intrinsics/builtins
        if function_name == "printString" {
            let print_fn = self.declare_print_string_fn();
            if args.len() != 1 {
                return Err(CodegenError::ArgumentError("printString expects 1 argument".to_string()));
            }
            let arg_val = self.codegen_expr(&args[0])?;
            if !arg_val.is_pointer_value() { // Espera i8*
                 return Err(CodegenError::TypeMismatch("printString argument must be a string".to_string()));
            }
            self.builder.build_call(print_fn, &[arg_val.into()], "call_puts");
            return Ok(None); // puts (printString) retorna int, mas tratamos como void no nível da linguagem
        } else if function_name == "printNumber" {
            let print_fn = self.declare_print_number_fn();
             if args.len() != 1 {
                return Err(CodegenError::ArgumentError("printNumber expects 1 argument".to_string()));
            }
            let arg_val = self.codegen_expr(&args[0])?;
             if !arg_val.is_int_value() { // Espera i32 para %d
                 return Err(CodegenError::TypeMismatch("printNumber argument must be a number".to_string()));
            }
            // Criar string de formato "%d\n"
            let format_str = self.builder.build_global_string_ptr("%d\n", ".format_d").unwrap();
            self.builder.build_call(print_fn, &[format_str.as_pointer_value().into(), arg_val.into()], "call_printf");
            return Ok(None);
        }

        // Chamada de função definida pelo usuário
        let function = self.module.get_function(function_name)
            .ok_or_else(|| CodegenError::UndefinedFunction(function_name.clone()))?;
        
        let mut compiled_args = Vec::new();
        for arg_expr in args {
            compiled_args.push(self.codegen_expr(arg_expr)?.into());
        }

        let call_site = self.builder.build_call(function, &compiled_args, &format!("call_{}", function_name))
             .map_err(|e| CodegenError::LlvmError(format!("Failed to build call: {}", e)))?; // Inkwell 0.4+

        Ok(call_site.try_as_basic_value().left())
    }


    // ... (codegen_statement e codegen_function ajustados para a nova AST)
    // codegen_program agora itera sobre ProgramNode.body que contém StatementNodes
     pub fn codegen_program(&mut self, program: &ProgramNode) -> CodegenResult<()> { // Alterado para não retornar Módulo diretamente
        for stmt_node in &program.body {
            match stmt_node {
                StatementNode::FunctionDeclaration(func_decl) => {
                    self.codegen_function(func_decl)?;
                }
                StatementNode::VariableDeclaration { .. } => {
                    // Variáveis globais - podem precisar de tratamento especial ou ser proibidas no MVP
                    // Para o MVP, podemos assumir que todas as vars são locais a funções.
                    // Ou, se `main` é a única função global, processar aqui.
                    return Err(CodegenError::NotImplemented("Global variable declarations not supported in MVP".to_string()));

                }
                // Outros statements no nível do programa (imports, etc. - não para MVP)
                _ => return Err(CodegenError::InvalidOperation("Only function declarations are expected at the top level of the program for MVP".to_string())),
            }
        }
        Ok(())
    }
    
    // Adicionar um método para obter o módulo após a geração
    pub fn into_module(self) -> Module<'ctx> {
        self.module
    }
}