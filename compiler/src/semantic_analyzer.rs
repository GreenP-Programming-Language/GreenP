use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, thiserror::Error)]
pub enum SemanticError {
    #[error("Undefined variable: '{0}'")]
    UndefinedVariable(String),
    #[error("Undefined function: '{0}'")]
    UndefinedFunction(String),
    #[error("Type mismatch: expected {expected}, found {found} in {context}")]
    TypeMismatch { expected: String, found: String, context: String },
    #[error("Argument count mismatch for function '{func_name}': expected {expected}, found {found}")]
    ArgumentCountMismatch { func_name: String, expected: usize, found: usize },
    // Adicionar mais erros semânticos conforme necessário
}

pub type SemanticResult<T> = Result<T, SemanticError>;

// Tabela de símbolos simplificada para o MVP
struct SymbolTable {
    // Poderia ser mais complexo com escopos, etc.
    variables: HashMap<String, TypeAnnotationNode>, // Nome da var -> Tipo
    functions: HashMap<String, (Vec<TypeAnnotationNode>, TypeAnnotationNode)>, // Nome da func -> (Tipos dos Params, Tipo de Retorno)
}

impl SymbolTable {
    fn new() -> Self {
        let mut functions = HashMap::new();
        // Adicionar intrínsecos
        functions.insert(
            "printString".to_string(),
            (vec![TypeAnnotationNode::String], TypeAnnotationNode::Void)
        );
        functions.insert(
            "printNumber".to_string(),
            (vec![TypeAnnotationNode::Number], TypeAnnotationNode::Void)
        );
        SymbolTable { variables: HashMap::new(), functions }
    }
    // Métodos para adicionar/buscar símbolos
}

pub struct SemanticAnalyzer {
    // Para MVP, uma única tabela de símbolos global/nível de função pode ser suficiente.
    // Em uma implementação completa, haveria uma pilha de tabelas para escopos.
    symbol_table_stack: Vec<SymbolTable>, // Para escopos futuros
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer { symbol_table_stack: vec![SymbolTable::new()] } // Inicia com escopo global
    }

    // O analisador semântico pode retornar uma AST anotada ou apenas Result<(), SemanticError>
    pub fn analyze_program(&mut self, program: &ProgramNode) -> SemanticResult<()> {
        for statement in &program.body {
            self.analyze_statement(statement)?;
        }
        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> SemanticResult<()> {
        match stmt {
            Statement::FunctionDeclaration(_func_decl) => {
                // TODO: Adicionar função à tabela de símbolos.
                // TODO: Criar novo escopo para parâmetros e corpo da função.
                // TODO: Analisar corpo da função.
                // TODO: Remover escopo.
                Ok(())
            }
            // Statement::VariableDeclaration { name, ty_annotation, initializer, .. } => {
            //     // TODO: Verificar se o tipo do inicializador (se houver) corresponde à anotação.
            //     // TODO: Adicionar variável à tabela de símbolos do escopo atual.
            //     Ok(())
            // }
            Statement::ExpressionStatement(expr) => {
                self.analyze_expression(expr)?;
                Ok(())
            }
            Statement::ReturnStatement(expr_opt) => {
                // TODO: Verificar se o tipo da expressão de retorno corresponde ao tipo de retorno da função atual.
                if let Some(expr) = expr_opt {
                    self.analyze_expression(expr)?;
                }
                Ok(())
            }
            // Ignorar outros para o MVP inicial ou retornar NotImplemented
            _ => Ok(()), // Ou Err(SemanticError::NotImplemented(...))
        }
    }

    fn analyze_expression(&mut self, expr: &ExpressionNode) -> SemanticResult<TypeAnnotationNode> {
        match expr {
            ExpressionNode::Literal(LiteralNode::Number(_)) => Ok(TypeAnnotationNode::Number),
            ExpressionNode::Literal(LiteralNode::String(_)) => Ok(TypeAnnotationNode::String),
            ExpressionNode::Literal(LiteralNode::Boolean(_)) => Ok(TypeAnnotationNode::Boolean),
            ExpressionNode::Identifier(id_node) => {
                // TODO: Buscar variável na tabela de símbolos e retornar seu tipo.
                // Se não encontrada, retornar SemanticError::UndefinedVariable.
                // Para MVP, pode ser um placeholder.
                // Exemplo simplificado:
                if let Some(ty) = self.current_scope().variables.get(&id_node.0) {
                    Ok(ty.clone())
                } else {
                    Err(SemanticError::UndefinedVariable(id_node.0.clone()))
                }
            }
            ExpressionNode::FunctionCall { callee, args } => {
                let (expected_param_types, return_type) = self.current_scope().functions.get(&callee.0)
                    .cloned() // Clona para evitar problemas de lifetime com o &mut self abaixo
                    .ok_or_else(|| SemanticError::UndefinedFunction(callee.0.clone()))?;

                if args.len() != expected_param_types.len() {
                    return Err(SemanticError::ArgumentCountMismatch {
                        func_name: callee.0.clone(),
                        expected: expected_param_types.len(),
                        found: args.len(),
                    });
                }

                for (arg_expr, expected_type) in args.iter().zip(expected_param_types.iter()) {
                    let arg_type = self.analyze_expression(arg_expr)?;
                    if arg_type != *expected_type { // Compara os tipos
                        return Err(SemanticError::TypeMismatch {
                            expected: format!("{:?}", expected_type),
                            found: format!("{:?}", arg_type),
                            context: format!("argument for function '{}'", callee.0),
                        });
                    }
                }
                Ok(return_type)
            }
            ExpressionNode::BinaryOp { op:_, left, right } => { // Simplificado para MVP
                let _left_type = self.analyze_expression(left)?;
                let _right_type = self.analyze_expression(right)?;
                // TODO: Verificar se os tipos são compatíveis com o operador.
                // Para MVP, se ambos são 'number', o resultado é 'number'.
                // if left_type == TypeAnnotationNode::Number && right_type == TypeAnnotationNode::Number {
                // Ok(TypeAnnotationNode::Number)
                // } else { Err(...) }
                Ok(TypeAnnotationNode::Number) // Assumindo que são números e o resultado é número
            }
            // _ => Err(SemanticError::NotImplemented("Expression analysis not fully implemented".to_string())),
        }
    }
    
    // Helper para obter o escopo atual (simplificado)
    fn current_scope(&self) -> &SymbolTable {
        self.symbol_table_stack.last().expect("Symbol table stack is empty")
    }
}