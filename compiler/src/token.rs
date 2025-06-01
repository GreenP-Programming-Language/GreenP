// src/token.rs
use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")] // Ignorar espaços em branco
#[logos(skip r"//[^\n]*")]   // Ignorar comentários de linha única
#[logos(skip r"/\*(?:[^*]|\*[^/])*\*/")] // Ignorar comentários de bloco
pub enum Token<'source> {
    // Keywords TypeScript-like
    #[token("function")]
    KwFunction,
    #[token("let")]
    KwLet,
    #[token("const")]
    KwConst,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("return")]
    KwReturn,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,

    // Types TypeScript-like
    #[token("number")]
    TyNumber,
    #[token("string")]
    TyString,
    #[token("boolean")]
    TyBoolean,
    #[token("void")]
    TyVoid,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'source str),

    #[regex("[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    IntegerLiteral(i64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice())] // Aspas duplas
    StringLiteral(&'source str),
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| lex.slice())] // Aspas simples (tratado como string)
    CharLikeStringLiteral(&'source str),


    // Operators and Punctuation
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("%")] Percent,

    #[token("===")] StrictEqual,
    #[token("!==")] StrictNotEqual,
    #[token("==")] EqualEqual,
    #[token("!=")] NotEqual,
    #[token("<")] LessThan,
    #[token(">")] GreaterThan,
    #[token("<=")] LessThanEqual,
    #[token(">=")] GreaterThanEqual,

    #[token("&&")] AmpersandAmpersand,
    #[token("||")] PipePipe,
    #[token("!")] Bang,

    #[token("=")] Assign,
    #[token(",")] Comma,
    #[token(";")] Semicolon,
    #[token(":")] Colon,
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LBrace,
    #[token("}")] RBrace,
}

impl<'source> fmt::Display for Token<'source> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum LexicalError {
    #[error("Unrecognized token at or near: {context}")]
    UnrecognizedToken { context: String },
    #[error("Invalid number literal: {value}")]
    InvalidNumberLiteral { value: String },
}