
pub mod token;
pub mod ast;
pub mod lexer; // Esta linha requer que src/lexer.rs (ou src/lexer/mod.rs) exista
pub mod parser;
pub mod semantic_analyzer;
pub mod codegen_llvm;
pub mod utils;

// Reexportar tipos de erro principais para serem acessíveis aos usuários da biblioteca
pub use token::LexicalError;
pub use parser::ParseError;
pub use semantic_analyzer::SemanticError;
pub use codegen_llvm::CodegenError;

// Esta 'use' é necessária porque ProgramNode é explicitamente anotado.
use ast::ProgramNode;

// As duas linhas abaixo foram removidas conforme sugerido pelo compilador,
// pois Token e Range não são nomeados explicitamente neste escopo.
// use token::Token;      // REMOVIDA
// use std::ops::Range;  // REMOVIDA

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;

use std::fs;
use std::path::Path;
use std::process::Command;

#[derive(Debug, thiserror::Error)]
pub enum GreenPCompilationError {
    #[error("I/O Error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Lexical Error: {0}")]
    Lex(#[from] LexicalError),
    #[error("Parse Error: {0}")]
    Parse(#[from] ParseError),
    #[error("Semantic Error: {0}")]
    Semantic(#[from] SemanticError),
    #[error("Codegen Error: {0}")]
    Codegen(#[from] CodegenError),
    #[error("LLVM Tooling Error: {0}")]
    LlvmTooling(String),
    #[error("Linking Error: {0}")]
    Linking(String),
    #[error("Internal Compiler Error: {0}")]
    Internal(String),
}

/// Compila um arquivo fonte GreenP para um executável.
// (Restante da função compile_file_to_executable como estava antes)
pub fn compile_file_to_executable(
    source_file_path: &str,
    output_executable_name: &str,
    print_diagnostics: bool,
) -> Result<(), GreenPCompilationError> {
    if print_diagnostics {
        println!("Compiling {} -> {}...", source_file_path, output_executable_name);
    }
    let source_code = fs::read_to_string(source_file_path)?;

    // 1. Lexing
    if print_diagnostics { println!("Phase 1: Lexing..."); }
    let tokens_with_spans = lexer::lex_source(&source_code)?; // O tipo é inferido aqui

    // 2. Parsing
    if print_diagnostics { println!("Phase 2: Parsing..."); }
    // Parser::new recebe &tokens_with_spans. O parser.rs lida com os tipos internos.
    let mut parser = parser::Parser::new(&tokens_with_spans);
    let ast_program: ProgramNode = parser.parse_program()?; // ProgramNode é usado explicitamente

    // 3. Semantic Analysis
    if print_diagnostics { println!("Phase 3: Semantic Analysis..."); }
    let mut semantic_analyzer = semantic_analyzer::SemanticAnalyzer::new();
    semantic_analyzer.analyze_program(&ast_program)?;

    // 4. Code Generation (LLVM IR)
    if print_diagnostics { println!("Phase 4: Generating LLVM IR..."); }
    let llvm_context = Context::create();
    let module_name = Path::new(source_file_path)
        .file_stem()
        .unwrap_or_default()
        .to_str()
        .unwrap_or("greenp_module");
    let mut codegen = codegen_llvm::CodeGenerator::new(&llvm_context, module_name);

    codegen.codegen_program(&ast_program)?;
    let llvm_module = codegen.into_module();

    if let Err(e) = llvm_module.verify() {
        return Err(GreenPCompilationError::LlvmTooling(format!(
            "LLVM Module verification failed: {}",
            e.to_string()
        )));
    }
    if print_diagnostics { println!("LLVM IR generated and verified."); }

    if let Some(main_fn) = llvm_module.get_function("main") {
        let fpm = PassManager::create(&llvm_module);
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.initialize();
        fpm.run_on(&main_fn);
    }

    // 5. Compilar LLVM IR para Arquivo Objeto
    // ... (restante da função como antes) ...
    if print_diagnostics { println!("Phase 5: Compiling LLVM IR to Object File..."); }
    Target::initialize_native(&InitializationConfig::default()).map_err(|e| {
        GreenPCompilationError::LlvmTooling(format!("Failed to initialize native target: {}", e))
    })?;

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).map_err(|e| {
        GreenPCompilationError::LlvmTooling(format!(
            "Failed to get target for triple {}: {}",
            triple.as_str().to_string_lossy(),
            e
        ))
    })?;

    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            &TargetMachine::get_host_cpu_features().to_string_lossy(),
            OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .ok_or_else(|| {
            GreenPCompilationError::LlvmTooling("Unable to create target machine".to_string())
        })?;

    let obj_file_name_stem = Path::new(output_executable_name)
        .file_stem()
        .unwrap_or_default()
        .to_str()
        .unwrap_or("output");
    let object_file_name = format!("{}.o", obj_file_name_stem);
    let object_file_path = Path::new(&object_file_name);

    target_machine
        .write_to_file(&llvm_module, inkwell::targets::FileType::Object, object_file_path)
        .map_err(|e| {
            GreenPCompilationError::LlvmTooling(format!(
                "Error writing object file '{}': {}",
                object_file_path.display(),
                e
            ))
        })?;
    if print_diagnostics { println!("Object file written to {}", object_file_path.display()); }

    // 6. Linkar Arquivo Objeto para criar Executável
    if print_diagnostics { println!("Phase 6: Linking object file to create executable {}...", output_executable_name); }
    let linker = option_env!("GREENP_CC").unwrap_or("cc");
    let output_cmd = Command::new(linker)
        .arg(object_file_path.to_str().ok_or_else(|| GreenPCompilationError::Internal("Object file path is not valid UTF-8".to_string()))?)
        .arg("-o")
        .arg(output_executable_name)
        .output()
        .map_err(|e| {
            GreenPCompilationError::Linking(format!("Failed to execute linker '{}': {}", linker, e))
        })?;

    if !output_cmd.status.success() {
        return Err(GreenPCompilationError::Linking(format!(
            "Linker failed with status {}:\nStdout: {}\nStderr: {}",
            output_cmd.status,
            String::from_utf8_lossy(&output_cmd.stdout),
            String::from_utf8_lossy(&output_cmd.stderr)
        )));
    }
    if print_diagnostics { println!("Executable {} created successfully.", output_executable_name); }

    Ok(())
}