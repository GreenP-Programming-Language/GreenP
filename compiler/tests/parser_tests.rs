use greenp_compiler::ast::*;
use greenp_compiler::token::Token;
use greenp_compiler::parser::{Parser, ParseError}; // Ajuste os caminhos
use greenp_compiler::lexer::lex_source; // Supondo uma função `lex_source` em `src/lexer.rs` que retorna Vec<(Token, Range)>

// Helper para lex + parse
fn parse_source_unwrap(source: &str) -> ProgramNode {
    let tokens_with_spans = lex_source(source) // Esta função deve existir e retornar Result<Vec<(Token, Range)>, LexicalError>
        .unwrap_or_else(|e| panic!("Lexing failed for test: {:?}, source: {}", e, source));
    let mut parser = Parser::new(&tokens_with_spans);
    parser.parse_program().unwrap_or_else(|e| panic!("Parsing failed for test: {:?}, source: {}", e, source))
}

#[test]
fn test_empty_program() {
    let source = "";
    let program = parse_source_unwrap(source);
    assert_eq!(program.body.len(), 0);
}

#[test]
fn test_simple_function_declaration_void_return() {
    let source = "function main(): void {}";
    let program = parse_source_unwrap(source);

    assert_eq!(program.body.len(), 1);
    match &program.body[0] {
        // O parser do MVP pode envolver FunctionDeclarationNode em algo como TopLevelStatement::FunctionDecl
        // ou a AST de ProgramNode pode ter um campo `functions: Vec<FunctionDeclarationNode>`
        // Ajuste conforme a AST real. Se ProgramNode.body é Vec<StatementNode>,
        // e StatementNode tem uma variante FunctionDeclaration:
        StatementNode::FunctionDeclaration(func_decl) => {
            assert_eq!(func_decl.name.0, "main");
            assert_eq!(func_decl.params.len(), 0);
            assert_eq!(func_decl.return_type, TypeAnnotationNode::Void);
            assert_eq!(func_decl.body.statements.len(), 0);
        }
        _ => panic!("Expected FunctionDeclarationNode"),
    }
}

#[test]
fn test_function_declaration_with_params_and_return_type() {
    let source = "function add(a: number, b: number): number { return a + b; }";
    let program = parse_source_unwrap(source);
    assert_eq!(program.body.len(), 1);
    // Adicionar asserções mais detalhadas para params, return type e corpo
}


#[test]
fn test_const_variable_declaration_with_initializer() {
    let source = "const message: string = \"Hello\";";
    let program = parse_source_unwrap(source);
    assert_eq!(program.body.len(), 1);
    match &program.body[0] {
        StatementNode::VariableDeclaration { is_const, name, ty_annotation, initializer } => {
            assert!(is_const);
            assert_eq!(name.0, "message");
            assert_eq!(ty_annotation, &Some(TypeAnnotationNode::String));
            assert!(initializer.is_some());
            // Adicionar asserções para o tipo de literal do inicializador
        }
        _ => panic!("Expected VariableDeclarationNode"),
    }
}

#[test]
fn test_let_variable_declaration() {
    let source = "let count: number;";
    let program = parse_source_unwrap(source);
    assert_eq!(program.body.len(), 1);
    // Asserções para is_const=false, sem inicializador
}


#[test]
fn test_function_call_statement_print_string() {
    let source = "function main(): void { printString(\"test\"); }";
    let program = parse_source_unwrap(source);
    // Navegar na AST para encontrar a chamada de printString e verificar seus argumentos
    // ...
}

#[test]
fn test_function_call_statement_print_number() {
    let source = "function main(): void { printNumber(123); }";
    let program = parse_source_unwrap(source);
    // Navegar na AST para encontrar a chamada de printNumber e verificar seus argumentos
    // ...
}

#[test]
fn test_expression_statement_addition() {
    // Supondo que a gramática permita expressões aritméticas simples no corpo da função
    let source = "function calculate(): number { let a:number = 5; let b:number = 10; return a + b; }";
    let program = parse_source_unwrap(source);
    // Verificar a estrutura da expressão 'a + b' dentro do return statement
    // ...
}


#[test]
fn test_parse_error_unexpected_token() {
    // Requer que `lex_source` e `Parser::new` estejam configurados para lidar com `Result`
    let source = "function main() void {}"; // Faltando ':' antes de 'void'
    let tokens_with_spans = lex_source(source).expect("Lexing should succeed");
    let mut parser = Parser::new(&tokens_with_spans);
    let result = parser.parse_program();
    assert!(result.is_err());
    if let Err(ParseError::UnexpectedToken { expected_desc, .. }) = result {
         assert!(expected_desc.contains("':'") || expected_desc.contains("type annotation") || expected_desc.contains("LBrace")); // Depende da mensagem de erro específica
    } else {
        panic!("Expected ParseError::UnexpectedToken, got {:?}", result);
    }
}

// Adicionar mais testes para diferentes construções, expressões, e casos de erro.