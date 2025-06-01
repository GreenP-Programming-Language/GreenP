// Em tests/semantic_tests.rs ou similar
use greenp_compiler::lexer::lex_source;
use greenp_compiler::parser::Parser;
use greenp_compiler::semantic_analyzer::{SemanticAnalyzer, SemanticError};

fn analyze_source_for_test(source: &str) -> Result<(), SemanticError> {
    let tokens = lex_source(source).unwrap();
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse_program().unwrap();
    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze_program(&ast)
}

#[test]
fn test_semantic_print_string_ok() {
    let source = "function main(): void { printString(\"ok\"); }";
    assert!(analyze_source_for_test(source).is_ok());
}

#[test]
fn test_semantic_print_string_type_error() {
    let source = "function main(): void { printString(123); }";
    let result = analyze_source_for_test(source);
    assert!(result.is_err());
    // Opcional: verificar o tipo específico do SemanticError
    // match result.unwrap_err() {
    //     SemanticError::TypeMismatch { .. } => { /* ok */ },
    //     _ => panic!("Expected TypeMismatch error"),
    // }
}
// Adicionar mais testes semânticos