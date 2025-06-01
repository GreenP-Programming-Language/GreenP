// tests/lexer_tests.rs
use greenp::lexer::lex_source; // Assumindo que lex_source está em greenp::lexer
use greenp::token::{Token, LexicalError};

// Helper para extrair apenas os tokens, ignorando spans, para facilitar asserções
fn get_tokens_from_source(source: &str) -> Result<Vec<Token>, LexicalError> {
    lex_source(source).map(|spanned_tokens| {
        spanned_tokens.into_iter().map(|(token, _span)| token).collect()
    })
}

#[test]
fn test_lex_keywords() {
    let source = "function let const return if else true false void number string boolean";
    let tokens = get_tokens_from_source(source).expect("Lexing keywords failed");
    assert_eq!(tokens, vec![
        Token::KwFunction, Token::KwLet, Token::KwConst, Token::KwReturn, Token::KwIf, Token::KwElse,
        Token::KwTrue, Token::KwFalse, Token::TyVoid, Token::TyNumber, Token::TyString, Token::TyBoolean,
    ]);
}

#[test]
fn test_lex_identifiers() {
    let source = "myVar another_var _var123 func_name";
    let tokens = get_tokens_from_source(source).expect("Lexing identifiers failed");
    assert_eq!(tokens, vec![
        Token::Identifier("myVar"), Token::Identifier("another_var"),
        Token::Identifier("_var123"), Token::Identifier("func_name"),
    ]);
}

#[test]
fn test_lex_literals() {
    let source = r#"123 0 "hello world" 'string too' true false"#;
    let tokens = get_tokens_from_source(source).expect("Lexing literals failed");
    assert_eq!(tokens, vec![
        Token::IntegerLiteral(123), Token::IntegerLiteral(0),
        Token::StringLiteral("\"hello world\""), // Logos mantém as aspas originais
        Token::CharLikeStringLiteral("'string too'"), // Logos mantém as aspas originais
        Token::KwTrue, Token::KwFalse,
    ]);
}

#[test]
fn test_lex_operators_and_punctuation() {
    let source = "+ - * / % = == === != !== < > <= >= && || ! ( ) { } [ ] : ; ,";
    // Adicione mais operadores conforme sua definição de Token
    let tokens = get_tokens_from_source(source).expect("Lexing operators failed");
    assert_eq!(tokens, vec![
        Token::Plus, Token::Minus, Token::Star, Token::Slash, Token::Percent,
        Token::Assign, Token::EqualEqual, Token::StrictEqual, Token::NotEqual, Token::StrictNotEqual,
        Token::LessThan, Token::GreaterThan, Token::LessThanEqual, Token::GreaterThanEqual,
        Token::AmpersandAmpersand, Token::PipePipe, Token::Bang,
        Token::LParen, Token::RParen, Token::LBrace, Token::RBrace,
        // Se você adicionar Token::LBracket, Token::RBracket ao seu lexer:
        // Token::LBracket, Token::RBracket, 
        Token::Colon, Token::Semicolon, Token::Comma,
    ]);
}

#[test]
fn test_lex_comments_and_whitespace() {
    let source = r#"
        // comment here
        function  main  ( )  {
            /* block
               comment */
            let x = 10; // another
        }
    "#;
    let tokens = get_tokens_from_source(source).expect("Lexing with comments failed");
    assert_eq!(tokens, vec![
        Token::KwFunction, Token::Identifier("main"), Token::LParen, Token::RParen, Token::LBrace,
        Token::KwLet, Token::Identifier("x"), Token::Assign, Token::IntegerLiteral(10), Token::Semicolon,
        Token::RBrace,
    ]);
}

#[test]
fn test_lex_unrecognized_token() {
    let source = "let a = #invalid_token;";
    let result = get_tokens_from_source(source);
    assert!(result.is_err());
    match result.unwrap_err() {
        LexicalError::UnrecognizedToken { context } => {
            assert!(context.contains('#'));
        }
        _ => panic!("Expected UnrecognizedToken error"),
    }
}