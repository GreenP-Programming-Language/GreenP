use greenp_compiler::ast::*;
use greenp_compiler::parser::Parser; // Ajuste os caminhos
use greenp_compiler::lexer::lex_source;
use greenp_compiler::semantic_analyzer::{SemanticAnalyzer, SemanticError}; // Supondo que exista

fn analyze_source(source: &str) -> Result<ProgramNode, SemanticError> { 
    let tokens_with_spans = lex_source(source)
        .unwrap_or_else(|e| panic!("Lexing failed for semantic test: {:?}, source: {}", e, source));
    let mut parser = Parser::new(&tokens_with_spans);
    let program_ast = parser.parse_program()
        .unwrap_or_else(|e| panic!("Parsing failed for semantic test: {:?}, source: {}", e, source));

    let mut semantic_analyzer = SemanticAnalyzer::new(); 
    semantic_analyzer.analyze_program(program_ast) // analyze_program pode retornar Result<AnnotatedProgramNode, SemanticError>
}


#[test]
fn test_valid_print_string_call() {
    let source = r#"
        function main(): void {
            printString("hello");
        }
    "#;
    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected semantic analysis to pass, but got: {:?}", result.err());
}

#[test]
fn test_invalid_print_string_call_wrong_type() {
    let source = r#"
        function main(): void {
            printString(123); // Erro: printString espera string
        }
    "#;
    let result = analyze_source(source);
    assert!(result.is_err());
    if let Err(SemanticError::TypeMismatch { .. }) = result.unwrap_err() {
        // Ok, tipo de erro esperado
    } else {
        panic!("Expected SemanticError::TypeMismatch for printString with number");
    }
}

#[test]
fn test_valid_print_number_call() {
    let source = r#"
        function main(): void {
            printNumber(42);
        }
    "#;
    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected semantic analysis to pass, but got: {:?}", result.err());
}

#[test]
fn test_invalid_print_number_call_wrong_type() {
    let source = r#"
        function main(): void {
            printNumber("error"); // Erro: printNumber espera número
        }
    "#;
    let result = analyze_source(source);
    assert!(result.is_err());
    if let Err(SemanticError::TypeMismatch { .. }) = result.unwrap_err() {
        // Ok
    } else {
        panic!("Expected SemanticError::TypeMismatch for printNumber with string");
    }
}

#[test]
fn test_variable_declaration_and_usage_ok() {
    let source = r#"
        function main(): void {
            const x: number = 10;
            printNumber(x);
        }
    "#;
    let result = analyze_source(source);
     assert!(result.is_ok(), "Expected semantic analysis to pass, but got: {:?}", result.err());
}

#[test]
fn test_undefined_variable_error() {
    let source = r#"
        function main(): void {
            printNumber(undeclaredVar); // Erro: variável não declarada
        }
    "#;
    let result = analyze_source(source);
    assert!(result.is_err());
     if let Err(SemanticError::UndefinedVariable { .. }) = result.unwrap_err() {
        // Ok
    } else {
        panic!("Expected SemanticError::UndefinedVariable, got {:?}", result.err());
    }
}

// TODO: Adicionar testes para checagem de tipos em atribuições, retornos de função, etc.,
//       quando o analisador semântico for mais completo.