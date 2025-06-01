
use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//[^\n]*")]
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
    #[token("number")] // Mapeado para i32 no MVP
    TyNumber,
    #[token("string")] // Mapeado para str_slice internamente
    TyString,
    #[token("boolean")]
    TyBoolean,
    #[token("void")]
    TyVoid,

    // Identifiers (incluindo console, log para possível tratamento futuro)
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'source str),

    // Literals
    #[regex("[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    IntegerLiteral(i64), // 'number' pode ser int ou float, MVP foca em int via i64

    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice())]
    StringLiteral(&'source str), // Aspas duplas
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| lex.slice())]
    CharLikeStringLiteral(&'source str), // Aspas simples para strings também

    // Operators and Punctuation
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("===")]
    StrictEqual,
    #[token("!==")]
    StrictNotEqual,
    #[token("==")] // Permitir, mas talvez desencorajar em favor de ===
    EqualEqual,
    #[token("!=")] // Permitir, mas talvez desencorajar em favor de !==
    NotEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessThanEqual,
    #[token(">=")]
    GreaterThanEqual,
    #[token("&&")]
    AmpersandAmpersand,
    #[token("||")]
    PipePipe,
    #[token("!")]
    Bang,
    #[token("=")]
    Assign,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    // Arrow for function types not in MVP function declaration, but good to have
    // #[token("=>")]
    // Arrow,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    // #[token(".")] // Dot operator, for future console.log like features
    // Dot,
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