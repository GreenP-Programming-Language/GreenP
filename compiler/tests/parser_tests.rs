// tests/parser_tests.rs
use greenp::lexer::lex_source;
use greenp::parser::{Parser, ParseError}; // Certifique-se que ParseError é público em parser
use greenp::ast::*;

// Helper para facilitar os testes de parsing
fn parse_test_source(source: &str) -> Result<ProgramNode, String> {
    let tokens = lex_source(source)
        .map_err(|e| format!("Lexer error: {:?}", e))?;
    let mut parser = Parser::new(&tokens);
    parser.parse_program()
        .map_err(|e| format!("Parser error: {:?}", e))
}

#[test]
fn test_parse_empty_program() {
    let ast = parse_test_source("").expect("Parsing empty program failed");
    assert!(ast.body.is_empty());
}

#[test]
fn test_parse_empty_statement() {
    let source = "function main(): void { ; }";
    let ast = parse_test_source(source).expect("Parsing empty statement failed");
    match &ast.body[0] {
        Statement::FunctionDeclaration(func_decl) => {
            assert_eq!(func_decl.body.statements.len(), 1);
            assert_eq!(func_decl.body.statements[0], Statement::Empty);
        }
        _ => panic!("Expected function declaration"),
    }
}

#[test]
fn test_parse_function_declaration() {
    let source = r#"
        function add(a: number, b: string): void {
            printString("hello");
        }
    "#;
    let ast = parse_test_source(source).expect("Parsing function declaration failed");
    assert_eq!(ast.body.len(), 1);
    match &ast.body[0] {
        Statement::FunctionDeclaration(func) => {
            assert_eq!(func.name.0, "add");
            assert_eq!(func.params.len(), 2);
            assert_eq!(func.params[0].name.0, "a");
            assert_eq!(func.params[0].ty_annotation, TypeAnnotationNode::Number);
            assert_eq!(func.params[1].name.0, "b");
            assert_eq!(func.params[1].ty_annotation, TypeAnnotationNode::String);
            assert_eq!(func.return_type, TypeAnnotationNode::Void);
            assert_eq!(func.body.statements.len(), 1); // printString("hello");
        }
        _ => panic!("Expected FunctionDeclaration"),
    }
}

#[test]
fn test_parse_variable_declaration() {
    let source = r#"
        function test(): void {
            const x: number = 10;
            let y: string = "test";
            let z: boolean;
        }
    "#;
    let ast = parse_test_source(source).expect("Parsing variable declarations failed");
     match &ast.body[0] {
        Statement::FunctionDeclaration(func) => {
            assert_eq!(func.body.statements.len(), 3);
            // Verificação do const x
            match &func.body.statements[0] {
                Statement::VariableDeclaration { is_const, name, ty_annotation, initializer } => {
                    assert_eq!(*is_const, true);
                    assert_eq!(name.0, "x");
                    assert_eq!(ty_annotation, &Some(TypeAnnotationNode::Number));
                    assert!(initializer.is_some());
                }
                _ => panic!("Expected const x declaration"),
            }
            // Adicionar verificações para y e z
        }
        _ => panic!("Expected FunctionDeclaration"),
    }
}

#[test]
fn test_parse_expression_statement_and_return() {
    let source = r#"
        function calc(): number {
            printNumber(1 + 2 * 3); // 1 + (2*3) = 7. Precisa de precedência no parser.
            return 42;
        }
    "#;
    // Nota: O parser MVP atual tem precedência simplificada. Este teste pode precisar de ajuste
    // ou ser usado para guiar a implementação de um parser com precedência (ex: Pratt).
    let ast = parse_test_source(source).expect("Parsing expression/return failed");
     match &ast.body[0] {
        Statement::FunctionDeclaration(func) => {
            assert_eq!(func.body.statements.len(), 2);
            // Verifica ExpressionStatement
            assert!(matches!(&func.body.statements[0], Statement::ExpressionStatement(_)));
            // Verifica ReturnStatement
            match &func.body.statements[1] {
                Statement::ReturnStatement(Some(ExpressionNode::Literal(LiteralNode::Number(42)))) => { /* ok */ }
                _ => panic!("Expected return 42;"),
            }
        }
        _ => panic!("Expected FunctionDeclaration"),
    }
}

#[test]
fn test_parser_syntax_error_unexpected_semicolon() {
    // Teste do erro que estávamos depurando
    let source = "function main(): void { printString(\"test\");; }"; // Ponto e vírgula duplo
    let result = parse_test_source(source);
    // Com Statement::Empty, isso agora deve ser parseado com sucesso,
    // resultando em dois statements: um ExpressionStatement e um Statement::Empty.
    // Se você NÃO quer permitir Statement::Empty e quer que isso seja um erro,
    // o teste deve ser assert!(result.is_err()).
    // Assumindo que Statement::Empty é permitido:
    assert!(result.is_ok(), "Double semicolon should parse with Statement::Empty. Error: {:?}", result.err());
    if let Ok(ast) = result {
        match &ast.body[0] {
            Statement::FunctionDeclaration(func) => {
                assert_eq!(func.body.statements.len(), 2);
                assert!(matches!(&func.body.statements[0], Statement::ExpressionStatement(_)));
                assert_eq!(func.body.statements[1], Statement::Empty);
            }
            _ => panic!("Expected function"),
        }
    }
}

#[test]
fn test_parser_syntax_error_in_expression() {
    let source = "function main(): void { let x: number = 10 + ; }"; // Erro na expressão
    let result = parse_test_source(source);
    assert!(result.is_err());
    // Você pode verificar o tipo específico de ParseError se quiser
}