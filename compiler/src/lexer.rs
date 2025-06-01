// src/lexer.rs
use crate::token::{Token, LexicalError};
use logos::Logos;
use std::ops::Range;

pub fn lex_source<'source>(source: &'source str) -> Result<Vec<(Token<'source>, Range<usize>)>, LexicalError> {
    Token::lexer(source)
        .spanned()
        .map(|(tok_res, span)| match tok_res {
            Ok(token) => Ok((token, span)),
            Err(_) => {
                let start = span.start.saturating_sub(10).min(source.len());
                let end = (span.end + 10).min(source.len());
                let context_slice = if start < end { &source[start..end] } else { "" };
                Err(LexicalError::UnrecognizedToken{ context: format!("near `...{}...` (span {:?})", context_slice, span) })
            }
        })
        .collect()
}