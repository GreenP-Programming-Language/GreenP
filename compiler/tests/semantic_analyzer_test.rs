// tests/semantic_analyzer_tests.rs
use greenp::lexer::lex_source;
use greenp::parser::Parser;
use greenp::ast::ProgramNode;
use greenp::semantic_analyzer::{SemanticAnalyzer, SemanticError, SemanticResult};

// Helper
fn analyze_source(source: &str) -> SemanticResult<()> {
    let tokens = lex_source(source).expect("Lexing failed during semantic test setup");
    let mut parser = Parser::new(&tokens);
    let ast: ProgramNode = parser.parse_program().expect("Parsing failed during semantic test setup");
    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze_program(&ast)
}

#[test]
fn semantic_valid_print_calls() {
    let source = r#"
        function main(): void {
            printString("hello");
            printNumber(123);
        }
    "#;
    assert!(analyze_source(source).is_ok());
}

#[test]
fn semantic_print_string_expects_string() {
    let source = "function main(): void { printString(123); }";
    let result = analyze_source(source);
    assert!(result.is_err());
    // Opcional: verificar o tipo de erro específico
    // match result.unwrap_err() {
    //     SemanticError::TypeMismatch { expected_type, found_type, .. } => {
    //         assert_eq!(expected_type, TypeAnnotationNode::String);
    //         assert_eq!(found_type, TypeAnnotationNode::Number);
    //     }
    //     _ => panic!("Expected TypeMismatch error for printString"),
    // }
}

#[test]
fn semantic_print_number_expects_number() {
    let source = "function main(): void { printNumber(\"error\"); }";
    assert!(analyze_source(source).is_err());
}

#[test]
fn semantic_variable_declaration_and_use() {
    let source = r#"
        function main(): void {
            const message: string = "hello";
            printString(message);
            let count: number = 0;
            count = count + 1;
            printNumber(count);
        }
    "#;
    assert!(analyze_source(source).is_ok());
}

#[test]
fn semantic_undefined_variable() {
    let source = "function main(): void { printNumber(undeclaredVar); }";
    let result = analyze_source(source);
    assert!(result.is_err());
    match result.unwrap_err() {
        SemanticError::UndefinedVariable(name) => assert_eq!(name, "undeclaredVar"),
        _ => panic!("Expected UndefinedVariable error"),
    }
}

#[test]
fn semantic_type_mismatch_in_assignment_or_init() {
    // Nota: A semântica de atribuição `count = count + 1` e a verificação de const
    // ainda não estão completas no SemanticAnalyzer do MVP.
    // Este teste pode falhar ou precisar de ajuste com base na implementação atual.
    let source = "function main(): void { const x: number = \"not a number\"; }";
    let result = analyze_source(source);
    assert!(result.is_err(), "Expected type mismatch for const x, got: {:?}", result.ok());
}

#[test]
fn semantic_return_type_mismatch() {
    let source = r#"
        function getNumber(): number {
            return "not a number"; // Error
        }
        function main(): void { getNumber(); }
    "#;
    // A análise de retorno dentro de getNumber pode precisar ser ativada/completada
    // no semantic_analyzer.
    assert!(analyze_source(source).is_err());
}

#[test]
fn semantic_void_function_returns_value() {
    let source = r#"
        function noReturn(): void {
            return 123; // Error
        }
        function main(): void { noReturn(); }
    "#;
    assert!(analyze_source(source).is_err());
}