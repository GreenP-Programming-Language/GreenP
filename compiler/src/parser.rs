// src/parser.rs
use crate::ast::*;
use crate::token::Token;
use std::iter::Peekable;
use std::ops::Range;

#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected_desc} but found {found_tok_desc} at {span:?}")]
    UnexpectedToken {
        expected_desc: String,
        found_tok_desc: String,
        span: Range<usize>,
    },
    #[error("Unexpected end of input while expecting {expected_desc}")]
    UnexpectedEOF { expected_desc: String },
    #[error("Invalid literal '{literal_text}' at {span:?}: {message}")]
    InvalidLiteral { literal_text: String, message: String, span: Range<usize> },
    #[error("Parser error: {message} at {span:?}")]
    General { message: String, span: Range<usize> },
}

type ParseResult<T> = Result<T, ParseError>;
type TokenStream<'a, 'source> = Peekable<std::slice::Iter<'a, (Token<'source>, Range<usize>)>>;

pub struct Parser<'a, 'source> {
    tokens: TokenStream<'a, 'source>,
    current_span: Range<usize>,
}

impl<'a, 'source> Parser<'a, 'source> {
    pub fn new(tokens_with_spans: &'a [(Token<'source>, Range<usize>)]) -> Self {
        Parser {
            tokens: tokens_with_spans.iter().peekable(),
            current_span: 0..0,
        }
    }

    fn peek_token_type(&mut self) -> Option<Token<'source>> {
        self.tokens.peek().map(|(token, _)| *token)
    }

    fn consume_token(&mut self) -> ParseResult<(Token<'source>, Range<usize>)> {
        match self.tokens.next() {
            Some((token, span)) => {
                self.current_span = span.clone();
                Ok((*token, span.clone()))
            }
            None => Err(ParseError::UnexpectedEOF { expected_desc: "any token".to_string() }),
        }
    }

    fn expect_token_specific(&mut self, expected: Token<'static>, expected_desc: &str) -> ParseResult<Range<usize>> {
        let (found_token, found_span) = self.consume_token()?;
        if std::mem::discriminant(&found_token) == std::mem::discriminant(&expected) {
            Ok(found_span)
        } else {
            Err(ParseError::UnexpectedToken {
                expected_desc: expected_desc.to_string(),
                found_tok_desc: format!("{:?}", found_token),
                span: found_span,
            })
        }
    }

    fn expect_identifier(&mut self) -> ParseResult<(String, Range<usize>)> {
        let (token, span) = self.consume_token()?;
        if let Token::Identifier(name) = token {
            Ok((name.to_string(), span))
        } else {
            Err(ParseError::UnexpectedToken{
                expected_desc: "identifier".to_string(),
                found_tok_desc: format!("{:?}", token),
                span
            })
        }
    }

    fn parse_type_annotation(&mut self) -> ParseResult<TypeAnnotationNode> {
        let (token, _span) = self.consume_token()?;
        match token {
            Token::TyNumber => Ok(TypeAnnotationNode::Number),
            Token::TyString => Ok(TypeAnnotationNode::String),
            Token::TyBoolean => Ok(TypeAnnotationNode::Boolean),
            Token::TyVoid => Ok(TypeAnnotationNode::Void),
            _ => Err(ParseError::UnexpectedToken{
                expected_desc: "type annotation (number, string, boolean, void)".to_string(),
                found_tok_desc: format!("{:?}", token),
                span: self.current_span.clone()
            })
        }
    }

    fn parse_literal(&mut self) -> ParseResult<LiteralNode> {
        let (token, span) = self.consume_token()?;
        match token {
            Token::IntegerLiteral(val) => Ok(LiteralNode::Number(val)),
            Token::StringLiteral(s) | Token::CharLikeStringLiteral(s) => {
                Ok(LiteralNode::String(s.trim_matches('"').trim_matches('\'').to_string()))
            }
            Token::KwTrue => Ok(LiteralNode::Boolean(true)),
            Token::KwFalse => Ok(LiteralNode::Boolean(false)),
            _ => Err(ParseError::InvalidLiteral{
                literal_text: format!("{:?}", token),
                message: "Not a valid literal token".to_string(),
                span
            })
        }
    }

    fn parse_primary_expression(&mut self) -> ParseResult<ExpressionNode> {
        match self.peek_token_type() {
            Some(Token::IntegerLiteral(_)) | Some(Token::StringLiteral(_)) |
            Some(Token::CharLikeStringLiteral(_)) | Some(Token::KwTrue) | Some(Token::KwFalse) => {
                Ok(ExpressionNode::Literal(self.parse_literal()?))
            }
            Some(Token::Identifier(_name_peeked)) => {
                let (name_str, _name_span) = self.expect_identifier()?;
                if self.peek_token_type() == Some(Token::LParen) {
                    self.consume_token()?; // Consume LParen '('
                    let mut args = Vec::new();
                    if self.peek_token_type() != Some(Token::RParen) {
                        loop {
                            args.push(self.parse_expression()?);
                            if self.peek_token_type() == Some(Token::Comma) {
                                self.consume_token()?; // Consume Comma ','
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect_token_specific(Token::RParen, ")")?;
                    Ok(ExpressionNode::FunctionCall { callee: IdentifierNode(name_str), args })
                } else {
                    Ok(ExpressionNode::Identifier(IdentifierNode(name_str)))
                }
            }
            Some(Token::LParen) => {
                self.consume_token()?; // Consume '('
                let expr = self.parse_expression()?;
                self.expect_token_specific(Token::RParen, ")")?;
                Ok(expr)
            }
            Some(other) => Err(ParseError::UnexpectedToken{
                expected_desc: "literal, identifier, or '('".to_string(),
                found_tok_desc: format!("{:?}", other),
                span: self.tokens.peek().map_or(self.current_span.clone(), |(_,s)|s.clone())
            }),
            None => Err(ParseError::UnexpectedEOF {expected_desc: "expression".to_string()})
        }
    }

    fn get_precedence(token: &Token) -> i32 {
        match token {
            Token::Star | Token::Slash | Token::Percent => 3,
            Token::Plus | Token::Minus => 2,
            Token::EqualEqual | Token::NotEqual | Token::StrictEqual | Token::StrictNotEqual |
            Token::LessThan | Token::GreaterThan | Token::LessThanEqual | Token::GreaterThanEqual => 1,
            _ => 0,
        }
    }

    fn parse_binary_op_rhs(&mut self, mut lhs: ExpressionNode, min_precedence: i32) -> ParseResult<ExpressionNode> {
        while let Some(op_token_peeked) = self.peek_token_type() {
            let precedence = Self::get_precedence(&op_token_peeked);
            if precedence < min_precedence || precedence == 0 {
                break;
            }

            let (op_token, _op_span) = self.consume_token()?;
            let mut rhs = self.parse_primary_expression()?;

            if let Some(next_op_peeked) = self.peek_token_type() {
                let next_precedence = Self::get_precedence(&next_op_peeked);
                // Para operadores com associatividade à direita ou maior precedência
                if precedence < next_precedence {
                    rhs = self.parse_binary_op_rhs(rhs, precedence + 1)?;
                }
                // Para associatividade à esquerda (mesma precedência), o loop externo cuida disso.
            }

            let binary_op = match op_token {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Sub,
                Token::Star => BinaryOperator::Mul,
                Token::Slash => BinaryOperator::Div,
                Token::Percent => BinaryOperator::Mod,
                Token::EqualEqual => BinaryOperator::Eq,
                Token::NotEqual => BinaryOperator::Neq,
                Token::StrictEqual => BinaryOperator::StrictEq,
                Token::StrictNotEqual => BinaryOperator::StrictNeq,
                Token::LessThan => BinaryOperator::Lt,
                Token::GreaterThan => BinaryOperator::Gt,
                Token::LessThanEqual => BinaryOperator::Lte,
                Token::GreaterThanEqual => BinaryOperator::Gte,
                _ => return Err(ParseError::General{ message: format!("Unexpected token {:?} found in binary operator position", op_token), span: _op_span }),
            };
            lhs = ExpressionNode::BinaryOp { op: binary_op, left: Box::new(lhs), right: Box::new(rhs) };
        }
        Ok(lhs)
    }

    fn parse_expression(&mut self) -> ParseResult<ExpressionNode> {
        let lhs = self.parse_primary_expression()?;
        self.parse_binary_op_rhs(lhs, 0)
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek_token_type() {
            Some(Token::KwFunction) => self.parse_function_declaration_statement(),
            Some(Token::KwLet) | Some(Token::KwConst) => self.parse_variable_declaration_statement(),
            Some(Token::KwReturn) => {
                self.consume_token()?; // Consome 'return'
                let expr = if self.peek_token_type() != Some(Token::Semicolon) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect_token_specific(Token::Semicolon, ";")?;
                Ok(Statement::ReturnStatement(expr))
            }
            Some(Token::LBrace) => Ok(Statement::Block(self.parse_block()?)),
            Some(Token::Semicolon) => {
                self.consume_token()?; // Consome o ;
                Ok(Statement::Empty)   // <<< CORRIGIDO AQUI
            }
            Some(_) => { // Default to ExpressionStatement
                let expr = self.parse_expression()?;
                self.expect_token_specific(Token::Semicolon, ";")?;
                Ok(Statement::ExpressionStatement(expr))
            }
            None => Err(ParseError::UnexpectedEOF { expected_desc: "statement".to_string()})
        }
    }

    fn parse_block(&mut self) -> ParseResult<BlockNode> {
        self.expect_token_specific(Token::LBrace, "{")?;
        let mut statements = Vec::new();
        while self.peek_token_type().is_some() && self.peek_token_type() != Some(Token::RBrace) {
            statements.push(self.parse_statement()?);
        }
        self.expect_token_specific(Token::RBrace, "}")?;
        Ok(BlockNode { statements })
    }

    fn parse_variable_declaration_statement(&mut self) -> ParseResult<Statement> {
        let (decl_token, _decl_span) = self.consume_token()?; // let ou const
        let is_const = decl_token == Token::KwConst;

        let (name_str, _name_span) = self.expect_identifier()?;

        let mut ty_annotation = None;
        if self.peek_token_type() == Some(Token::Colon) {
            self.consume_token()?; // Consome ':'
            ty_annotation = Some(self.parse_type_annotation()?);
        }

        let mut initializer = None;
        if self.peek_token_type() == Some(Token::Assign) {
            self.consume_token()?; // Consome '='
            initializer = Some(self.parse_expression()?);
        }
        self.expect_token_specific(Token::Semicolon, ";")?;

        Ok(Statement::VariableDeclaration {
            is_const,
            name: IdentifierNode(name_str),
            ty_annotation,
            initializer
        })
    }

    fn parse_function_declaration_statement(&mut self) -> ParseResult<Statement> {
        self.expect_token_specific(Token::KwFunction, "function keyword")?;
        let (name_str, _name_span) = self.expect_identifier()?;

        self.expect_token_specific(Token::LParen, "(")?;
        let mut params = Vec::new();
        if self.peek_token_type() != Some(Token::RParen) {
            loop {
                let (param_name_str, _p_name_span) = self.expect_identifier()?;
                self.expect_token_specific(Token::Colon, ": for parameter type")?;
                let param_ty = self.parse_type_annotation()?;
                params.push(FunctionParameterNode { name: IdentifierNode(param_name_str), ty_annotation: param_ty });
                if self.peek_token_type() == Some(Token::Comma) {
                    self.consume_token()?; // Consome Comma ','
                } else {
                    break;
                }
            }
        }
        self.expect_token_specific(Token::RParen, ")")?;

        let return_type = if self.peek_token_type() == Some(Token::Colon) {
            self.consume_token()?; // Consome ':'
            self.parse_type_annotation()?
        } else {
            TypeAnnotationNode::Void // Default para void se não especificado
        };

        let body = self.parse_block()?;

        Ok(Statement::FunctionDeclaration(FunctionDeclarationNode {
            name: IdentifierNode(name_str),
            params,
            return_type,
            body,
        }))
    }

    pub fn parse_program(&mut self) -> ParseResult<ProgramNode> {
        let mut body_statements = Vec::new();
        while self.peek_token_type().is_some() { // Continua enquanto houver tokens
            body_statements.push(self.parse_statement()?);
        }
        Ok(ProgramNode { body: body_statements })
    }
}