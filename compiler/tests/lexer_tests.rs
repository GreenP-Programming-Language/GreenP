
use greenp_compiler::token::{Token, LexicalError}; 
use logos::Logos; // Se você estiver usando a função lex_source diretamente, ela já usa Logos.

// Função helper para facilitar a escrita dos testes de lexing
fn lex_source_unwrap<'source>(source: &'source str) -> Vec<Token<'source>> {
    Token::lexer(source)
        .map(|res| res.unwrap_or_else(|_| panic!("Lexing failed for: {}", source))) // unwrap para testes é aceitável
        .collect()
}


#[test]
fn test_keywords() {
    let source = "function let const if else return true false number string boolean void";
    let tokens = lex_source_unwrap(source);
    assert_eq!(tokens, vec![
        Token::KwFunction, Token::KwLet, Token::KwConst, Token::KwIf, Token::KwElse,
        Token::KwReturn, Token::KwTrue, Token::KwFalse, Token::TyNumber, Token::TyString,
        Token::TyBoolean, Token::TyVoid,
    ]);
}

#[test]
fn test_identifiers() {
    let source = "myVar _anotherVar var123";
    let tokens = lex_source_unwrap(source);
    assert_eq!(tokens, vec![
        Token::Identifier("myVar"),
        Token::Identifier("_anotherVar"),
        Token::Identifier("var123"),
    ]);
}

#[test]
fn test_integer_literals() {
    let source = "123 0 98765";
    let tokens = lex_source_unwrap(source);
    assert_eq!(tokens, vec![
        Token::IntegerLiteral(123),
        Token::IntegerLiteral(0),
        Token::IntegerLiteral(98765),
    ]);
}

#[test]
fn test_string_literals() {
    let source = r#""hello" 'world'"#; // GreenP MVP aceita aspas duplas e simples para strings
    let tokens = lex_source_unwrap(source);
    assert_eq!(tokens, vec![
        Token::StringLiteral("\"hello\""), // Logos::Logos mantém as aspas
        Token::CharLikeStringLiteral("'world'"),
    ]);
}

#[test]
fn test_operators_and_punctuation() {
    let source = "+ - * / % === !== == != < > <= >= && || ! = , ; : ( ) { }";
    let tokens = lex_source_unwrap(source);
    assert_eq!(tokens, vec![
        Token::Plus, Token::Minus, Token::Star, Token::Slash, Token::Percent,
        Token::StrictEqual, Token::StrictNotEqual, Token::EqualEqual, Token::NotEqual,
        Token::LessThan, Token::GreaterThan, Token::LessThanEqual, Token::GreaterThanEqual,
        Token::AmpersandAmpersand, Token::PipePipe, Token::Bang, Token::Assign,
        Token::Comma, Token::Semicolon, Token::Colon, Token::LParen, Token::RParen,
        Token::LBrace, Token::RBrace,
    ]);
}

#[test]
fn test_comments_and_whitespace() {
    let source = r#"
        // Este é um comentário de linha
        function  myFunc  ( )  { /* comentário de bloco */ }
        let x = 10; // outro comentário
    "#;
    let tokens = lex_source_unwrap(source);
    assert_eq!(tokens, vec![
        Token::KwFunction, Token::Identifier("myFunc"), Token::LParen, Token::RParen, Token::LBrace, Token::RBrace,
        Token::KwLet, Token::Identifier("x"), Token::Assign, Token::IntegerLiteral(10), Token::Semicolon,
    ]);
}

#[test]
fn test_lexical_error_unrecognized_token() {
    use greenp_compiler::lexer::lex_source;
    let source = "let a = 10 $ 20;"; 
    let result = lex_source(source); 
    
    assert!(result.is_err());
    if let Err(LexicalError::UnrecognizedToken { context }) = result {
        assert!(context.contains('$'));
    } else {
        panic!("Expected LexicalError::UnrecognizedToken, got {:?}", result);
    }
}