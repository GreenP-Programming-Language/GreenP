// tests/common.rs

// use greenp_compiler::ast::ProgramNode; // Exemplo se você precisar de helpers que usam tipos da AST
// use greenp_compiler::lexer::lex_source;
// use greenp_compiler::parser::Parser;
// use greenp_compiler::token::LexicalError;
// use greenp_compiler::parser::ParseError;

// Exemplo de função auxiliar que poderia ser útil
// pub fn parse_test_source(source: &str) -> Result<ProgramNode, String> {
//     let tokens = lex_source(source)
//         .map_err(|e: LexicalError| format!("Lexer error: {}", e))?;
//     let mut parser = Parser::new(&tokens);
//     parser.parse_program()
//         .map_err(|e: ParseError| format!("Parser error: {}", e))
// }

// Por enquanto, pode ficar vazio ou com funções que você achar úteis em múltiplos testes.
#[allow(dead_code)]
pub fn setup_test_environment() {
    // println!("Setting up test environment...");
}