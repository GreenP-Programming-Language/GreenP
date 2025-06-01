// src/parser.rs
use crate::ast::*;
use crate::token::Token;
use std::iter::Peekable;
use crate::token::{Token, LexicalError};
use logos::Logos;

// Usar um tipo que embute o span para melhor error reporting depois
type TokenStream<'a, 'source> = Peekable<std::slice::Iter<'a, (Token<'source>, std::ops::Range<usize>)>>;


#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected_desc} but found {found_tok:?} at {span:?}")]
    UnexpectedToken {
        expected_desc: String,
        found_tok: Option<Token<'static>>, // 'static para fins de erro
        span: std::ops::Range<usize>,
    },
    #[error("Unexpected end of input while expecting {expected_desc}")]
    UnexpectedEOF { expected_desc: String },
    #[error("Invalid literal '{literal_text}' at {span:?}: {message}")]
    InvalidLiteral { literal_text: String, message: String, span: std::ops::Range<usize> },
    #[error("Parser error: {message} at {span:?}")]
    General { message: String, span: std::ops::Range<usize> },
}

pub fn lex_source<'source>(source: &'source str) -> Result<Vec<(Token<'source>, std::ops::Range<usize>)>, LexicalError> {
    Token::lexer(source)
        .spanned() // Para obter os ranges/spans dos tokens
        .map(|(tok_res, span)| match tok_res {
            Ok(token) => Ok((token, span)),
            Err(_) => {
                // Fornece um contexto melhor para o erro
                let start = span.start.saturating_sub(10);
                let end = (span.end + 10).min(source.len());
                let context_slice = &source[start..end];
                Err(LexicalError::UnrecognizedToken{ context: format!("...{}...", context_slice) })
            }
        })
        .collect()
}

// Helper para converter Token<'source> para Token<'static> para mensagens de erro
fn to_static_token(token: &Token<'_>) -> Token<'static> {
    match token {
        Token::Identifier(s) => Token::Identifier("ident"), // Placeholder
        Token::IntegerLiteral(_) => Token::IntegerLiteral(0),
        Token::StringLiteral(s) => Token::StringLiteral("string"),
        Token::CharLikeStringLiteral(s) => Token::CharLikeStringLiteral("string"),
        // Copiar outros tokens
        t => *t, // Para tokens que são Copy
    }
}


type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a, 'source> {
    tokens: TokenStream<'a, 'source>,
    current_span: std::ops::Range<usize>, // Rastrear o span do último token consumido
}

impl<'a, 'source> Parser<'a, 'source> {
    pub fn new(tokens: &'a [(Token<'source>, std::ops::Range<usize>)]) -> Self {
        Parser {
            tokens: tokens.iter().peekable(),
            current_span: 0..0,
        }
    }

    fn consume_token_if(&mut self, p: impl FnOnce(&Token<'source>) -> bool) -> Option<(Token<'source>, std::ops::Range<usize>)> {
        if self.tokens.peek().map_or(false, |(tok, _)| p(tok)) {
            let (token, span) = self.tokens.next().unwrap().clone(); // Clone token e span
            self.current_span = span.clone();
            Some((token, span))
        } else {
            None
        }
    }
    
    fn expect_token_variant(&mut self, variant_discriminant: std::mem::Discriminant<Token<'source>>, expected_desc: &str) -> ParseResult<(Token<'source>, std::ops::Range<usize>)> {
        match self.tokens.peek() {
            Some((peeked_token, _)) if std::mem::discriminant(peeked_token) == variant_discriminant => {
                let (token, span) = self.tokens.next().unwrap().clone();
                self.current_span = span.clone();
                Ok((token, span))
            }
            Some((peeked_token, peeked_span)) => Err(ParseError::UnexpectedToken {
                expected_desc: expected_desc.to_string(),
                found_tok: Some(to_static_token(peeked_token)),
                span: peeked_span.clone(),
            }),
            None => Err(ParseError::UnexpectedEOF { expected_desc: expected_desc.to_string() }),
        }
    }


    fn parse_type_annotation(&mut self) -> ParseResult<(TypeAnnotationNode, std::ops::Range<usize>)> {
        self.expect_token_variant(std::mem::discriminant(&Token::Colon("")), ":")?;
        let (type_token, type_span) = match self.tokens.next() {
            Some((tok @ Token::TyNumber, span)) => Ok((TypeAnnotationNode::Number, span.clone())),
            Some((tok @ Token::TyString, span)) => Ok((TypeAnnotationNode::String, span.clone())),
            Some((tok @ Token::TyBoolean, span)) => Ok((TypeAnnotationNode::Boolean, span.clone())),
            Some((tok @ Token::TyVoid, span)) => Ok((TypeAnnotationNode::Void, span.clone())),
            Some((other_tok, other_span)) => Err(ParseError::UnexpectedToken {
                expected_desc: "type annotation (number, string, boolean, void)".to_string(),
                found_tok: Some(to_static_token(other_tok)),
                span: other_span.clone(),
            }),
            None => Err(ParseError::UnexpectedEOF { expected_desc: "type annotation".to_string() }),
        }?;
        self.current_span = type_span.clone();
        Ok((type_token, type_span))
    }

    // Simplificado para MVP
    fn parse_primary_expression(&mut self) -> ParseResult<ExpressionNode> {
        let (next_tok, tok_span) = self.tokens.next().ok_or_else(|| ParseError::UnexpectedEOF { expected_desc: "expression".to_string() })?;
        self.current_span = tok_span.clone();

        match next_tok {
            Token::IntegerLiteral(val) => Ok(ExpressionNode::Literal(LiteralNode::Number(*val))),
            Token::StringLiteral(s) | Token::CharLikeStringLiteral(s) => {
                 Ok(ExpressionNode::Literal(LiteralNode::String(s.trim_matches('"').trim_matches('\'').to_string())))
            }
            Token::KwTrue => Ok(ExpressionNode::Literal(LiteralNode::Boolean(true))),
            Token::KwFalse => Ok(ExpressionNode::Literal(LiteralNode::Boolean(false))),
            Token::Identifier(name) => {
                // Verificar se é uma chamada de função
                if self.tokens.peek().map_or(false, |(t, _)| *t == Token::LParen) {
                    self.tokens.next(); // Consumir LParen
                    let mut args = Vec::new();
                    // Simplificado: parse de argumentos
                    if !self.tokens.peek().map_or(false, |(t,_)| *t == Token::RParen) {
                        args.push(self.parse_expression()?); // Parse do primeiro argumento
                        while self.tokens.peek().map_or(false, |(t,_)| *t == Token::Comma) {
                            self.tokens.next(); // Consumir Comma
                             args.push(self.parse_expression()?);
                        }
                    }
                    self.expect_token_variant(std::mem::discriminant(&Token::RParen), ")")?;
                    Ok(ExpressionNode::FunctionCall { callee: IdentifierNode(name.to_string()), args })
                } else {
                    Ok(ExpressionNode::Identifier(IdentifierNode(name.to_string())))
                }
            }
            // Adicionar LParen para expressões agrupadas
            _ => Err(ParseError::UnexpectedToken {
                expected_desc: "primary expression (literal, identifier, function call)".to_string(),
                found_tok: Some(to_static_token(next_tok)),
                span: tok_span.clone(),
            })
        }
    }
    
    // Implementar parse_expression com precedência de operadores (Pratt parser é bom aqui)
    // Por enquanto, um placeholder muito simples para o MVP
    fn parse_expression(&mut self) -> ParseResult<ExpressionNode> {
        let lhs = self.parse_primary_expression()?;

        // Checar por operadores binários (simplificado, sem precedência correta)
        if let Some((op_tok, _)) = self.tokens.peek() {
            let op = match op_tok {
                Token::Plus => Some(BinaryOperator::Add),
                Token::Minus => Some(BinaryOperator::Sub),
                Token::Star => Some(BinaryOperator::Mul),
                Token::Slash => Some(BinaryOperator::Div),
                // Adicionar mais operadores
                _ => None,
            };

            if let Some(op_variant) = op {
                self.tokens.next(); // Consumir o operador
                let rhs = self.parse_expression()?; // recursão simples, pode dar problema de associatividade/precedência
                return Ok(ExpressionNode::BinaryOp { op: op_variant, left: Box::new(lhs), right: Box::new(rhs) });
            }
        }
        Ok(lhs)
    }


    fn parse_statement(&mut self) -> ParseResult<StatementNode> {
        let (peeked_tok, peeked_span) = self.tokens.peek().ok_or_else(|| ParseError::UnexpectedEOF { expected_desc: "statement".to_string() })?.clone();
        // Não consumir ainda, apenas espiar

        match peeked_tok {
            Token::KwFunction => Ok(self.parse_function_declaration_statement()?),
            Token::KwLet | Token::KwConst => Ok(self.parse_variable_declaration_statement()?),
            Token::KwReturn => {
                self.tokens.next(); // Consumir 'return'
                let expr = if self.tokens.peek().map_or(false, |(t,_)| *t != Token::Semicolon) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect_token_variant(std::mem::discriminant(&Token::Semicolon), ";")?;
                Ok(StatementNode::ReturnStatement(expr))
            }
            // ... outros tipos de statement
            _ => { // Default to ExpressionStatement
                let expr = self.parse_expression()?;
                self.expect_token_variant(std::mem::discriminant(&Token::Semicolon), ";")?;
                Ok(StatementNode::ExpressionStatement(expr))
            }
        }
    }

    fn parse_block(&mut self) -> ParseResult<BlockNode> {
        self.expect_token_variant(std::mem::discriminant(&Token::LBrace), "{")?;
        let mut statements = Vec::new();
        while !self.tokens.peek().map_or(true, |(t,_)| *t == Token::RBrace) {
            statements.push(self.parse_statement()?);
        }
        self.expect_token_variant(std::mem::discriminant(&Token::RBrace), "}")?;
        Ok(BlockNode { statements })
    }
    
    fn parse_variable_declaration_statement(&mut self) -> ParseResult<StatementNode> {
        let (decl_tok, _) = self.tokens.next().unwrap(); // let ou const
        let is_const = *decl_tok == Token::KwConst;

        let (name_tok, name_span) = self.expect_token_variant(std::mem::discriminant(&Token::Identifier("")), "identifier")?;
        let name_str = match name_tok { Token::Identifier(s) => s.to_string(), _ => unreachable!() };
        
        let mut ty_annotation = None;
        if self.tokens.peek().map_or(false, |(t,_)| *t == Token::Colon) {
            ty_annotation = Some(self.parse_type_annotation()?.0);
        }

        let mut initializer = None;
        if self.tokens.peek().map_or(false, |(t,_)| *t == Token::Assign) {
            self.tokens.next(); // Consumir '='
            initializer = Some(self.parse_expression()?);
        }
        self.expect_token_variant(std::mem::discriminant(&Token::Semicolon), ";")?;

        Ok(StatementNode::VariableDeclaration { 
            is_const, 
            name: IdentifierNode(name_str), 
            ty_annotation, 
            initializer 
        })
    }


    fn parse_function_declaration_statement(&mut self) -> ParseResult<StatementNode> {
        self.expect_token_variant(std::mem::discriminant(&Token::KwFunction), "function")?;
        let (name_tok, _) = self.expect_token_variant(std::mem::discriminant(&Token::Identifier("")), "function name")?;
        let name_str = match name_tok { Token::Identifier(s) => s.to_string(), _ => unreachable!() };

        self.expect_token_variant(std::mem::discriminant(&Token::LParen), "(")?;
        let mut params = Vec::new();
        // Parse de parâmetros (simplificado)
         while !self.tokens.peek().map_or(true, |(t,_)| *t == Token::RParen) {
            let (param_name_tok, _) = self.expect_token_variant(std::mem::discriminant(&Token::Identifier("")), "parameter name")?;
            let param_name_str = match param_name_tok { Token::Identifier(s) => s.to_string(), _ => unreachable!() };
            let (param_ty, _) = self.parse_type_annotation()?;
            params.push(FunctionParameterNode { name: IdentifierNode(param_name_str), ty_annotation: param_ty });
            if self.tokens.peek().map_or(false, |(t,_)| *t == Token::Comma) {
                self.tokens.next(); // Consumir Comma
            } else {
                break;
            }
        }
        self.expect_token_variant(std::mem::discriminant(&Token::RParen), ")")?;
        
        let (return_type, _) = if self.tokens.peek().map_or(false, |(t,_)| *t == Token::Colon) {
            self.parse_type_annotation()?
        } else {
            // Default to void if no return type is specified, or error if strict typing needed
            (TypeAnnotationNode::Void, self.current_span.clone()) // Default implicit void
        };

        let body = self.parse_block()?;
        
        Ok(StatementNode::Block(BlockNode { // Em TS, func decl é um statement, mas nossa AST pode envolvê-lo
            statements: vec![
                StatementNode::ExpressionStatement(ExpressionNode::Identifier(IdentifierNode("placeholder_for_func_decl_in_block_Wrapper".into()))), // FIXME
            ] // This is not quite right. parse_program should expect FunctionDeclarationNode directly or as part of Program body.
        }))
        // O correto seria a ProgramNode ter FunctionDeclarationNode, não StatementNode->Block->Expr(Ident)
        // Ou ter um FunctionDeclaration StatementNode variant. Let's add that:
        // StatementNode::FunctionDeclaration(FunctionDeclarationNode { ... })
        // Para MVP, vamos simplificar e assumir que `parse_program` chama uma `parse_top_level_statement`
        // que pode ser uma declaração de função.
    }
    
    // Ajustar parse_program para construir ProgramNode { body: Vec<StatementNode> }
    // Onde StatementNode pode ser uma FunctionDeclaration, VariableDeclaration, etc.
    pub fn parse_program(&mut self) -> ParseResult<ProgramNode> {
        let mut body = Vec::new();
        while self.tokens.peek().is_some() {
            body.push(self.parse_statement()?);
        }
        Ok(ProgramNode { body })
    }
}