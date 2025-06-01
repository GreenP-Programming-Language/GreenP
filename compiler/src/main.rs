
fn compile_and_run(source_file: &str, output_executable: &str) -> Result<(), CompilationError> {
    // ... (lexing, parsing como antes, usando a nova AST e tokens)
    // let tokens_with_spans = lex_source(&source_code).map_err(CompilationError::Lex)?;
    // let mut parser = Parser::new(&tokens_with_spans);
    // let ast_program = parser.parse_program()?;


    // 3. Code Generation (LLVM IR)
    println!("Generating LLVM IR...");
    let context = Context::create();
    let module_name = Path::new(source_file).file_stem().unwrap_or_default().to_str().unwrap_or("greenp_module");
    let mut codegen = CodeGenerator::new(&context, module_name);
    
    // Ajustar para a nova AST: ProgramNode contém StatementNodes,
    // e assumimos que parse_program retorna ProgramNode.
    // O parser precisa ser ajustado para produzir FunctionDeclarationNode dentro de ProgramNode.body.
    // Para simplificar, vamos assumir que o ProgramNode tem uma lista de FunctionDeclarationNodes por agora.
    // (A AST de StatementNode::FunctionDeclaration precisaria ser extraída pelo parser para popular isso)
    
    // Para o MVP, vamos refatorar o parser para que ProgramNode contenha diretamente `functions: Vec<FunctionDeclarationNode>`.
    // E o codegen_program iteraria sobre isso.
    // Se ProgramNode.body contém StatementNode::FunctionDeclaration, o codegen precisa extraí-los.

    // Supondo que codegen.codegen_program() agora preenche o módulo internamente:
    codegen.codegen_program(&ast_program).map_err(CompilationError::from)?; // ast_program é ProgramNode
    
    let llvm_module = codegen.into_module(); // Obter o módulo após a geração

    // ... (otimização, compilação para objeto, linkagem como antes)
    // A função 'main' da GreenP (TS-like) será o entry point.
     if let Some(main_fn) = llvm_module.get_function("main") {
         // No inkwell 0.4, run_passes é um método do PassManager, não do CodeGenerator
         let fpm = PassManager::create(&llvm_module);
            fpm.add_instruction_combining_pass();
            fpm.add_reassociate_pass();
            // ... outras passes
            fpm.initialize();
            fpm.run_on(&main_fn); // Executar passes na função 'main'
        println!("'main' function optimized (if found).");
    }


    // ... (resto como antes)
    Ok(())
}

// ... (main function com exemplo de código GreenP atualizado)
fn main() {
    let greenp_ts_like_example = r#"
function main(): void {
  const message: string = "Hello from GreenP (TypeScript-Style Syntax)!";
  printString(message);
  
  let x: number = 100;
  const y: number = 50;
  let result: number = x + y;
  printNumber(result); // Esperado: 150
  
  result = x - y;
  printNumber(result); // Esperado: 50
}
"#;
    fs::write("hello.gp", greenp_ts_like_example).expect("Unable to write hello.gp");

    match compile_and_run("hello.gp", "hello_greenp_ts_mvp") {
        Ok(()) => {
            println!("\nCompilation successful! Running ./hello_greenp_ts_mvp");
            let run_output = Command::new("./hello_greenp_ts_mvp").output().expect("Failed to run executable");
            println!("Output:\n{}", String::from_utf8_lossy(&run_output.stdout));
            if !run_output.stderr.is_empty() {
                 eprintln!("Stderr:\n{}", String::from_utf8_lossy(&run_output.stderr));
            }
        }
        Err(e) => {
            eprintln!("\nCompilation failed: {}", e);
            // Imprimir a causa raiz se disponível (com 'thiserror')
            let mut cause = e.source();
            while let Some(source) = cause {
                eprintln!("  Caused by: {}", source);
                cause = source.source();
            }
        }
    }
}