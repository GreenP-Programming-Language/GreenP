// src/semantic_analyzer.rs
use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, thiserror::Error)]
pub enum SemanticError {
    #[error("Undefined variable: '{0}' in current scope")]
    UndefinedVariable(String),
    #[error("Undefined function: '{0}'")]
    UndefinedFunction(String),
    #[error("Type mismatch: expected {expected_type:?}, found {found_type:?} in {context}")]
    TypeMismatch { expected_type: TypeAnnotationNode, found_type: TypeAnnotationNode, context: String },
    #[error("Argument count mismatch for function '{func_name}': expected {expected_count}, found {found_count}")]
    ArgumentCountMismatch { func_name: String, expected_count: usize, found_count: usize },
    #[error("Cannot assign to a 'const' variable: '{0}'")]
    ConstAssignment(String), // Placeholder for future
    #[error("Feature not implemented in semantic analyzer: {0}")]
    NotImplemented(String),
}

pub type SemanticResult<T> = Result<T, SemanticError>;

#[derive(Clone)] // Symbol table entry needs to be cloneable if table is cloned for scopes
struct VariableInfo {
    ty: TypeAnnotationNode,
    is_const: bool,
}

#[derive(Clone)]
struct FunctionSignature {
    param_types: Vec<TypeAnnotationNode>,
    return_type: TypeAnnotationNode,
}

// For MVP, a single scope. Real implementation needs a stack of HashMaps for scopes.
#[derive(Clone, Default)]
struct Scope {
    variables: HashMap<String, VariableInfo>,
    functions: HashMap<String, FunctionSignature>,
}

impl Scope {
    fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert(
            "printString".to_string(),
            FunctionSignature {
                param_types: vec![TypeAnnotationNode::String],
                return_type: TypeAnnotationNode::Void,
            }
        );
        functions.insert(
            "printNumber".to_string(),
            FunctionSignature {
                param_types: vec![TypeAnnotationNode::Number],
                return_type: TypeAnnotationNode::Void,
            }
        );
        Scope { variables: HashMap::new(), functions }
    }
}

pub struct SemanticAnalyzer {
    // For a full implementation, this would be a stack of scopes.
    // current_scope: Scope,
    scopes: Vec<Scope>, // Stack of scopes
    current_function_return_type: Option<TypeAnnotationNode>, // For checking return statements
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer { 
            scopes: vec![Scope::new()], // Start with a global scope
            current_function_return_type: None,
        }
    }
    
    fn enter_scope(&mut self) {
        self.scopes.push(self.scopes.last().cloned().unwrap_or_default()); // Inherit for function/block scope
    }

    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 { // Don't pop the global scope
            self.scopes.pop();
        }
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("Scope stack is empty")
    }
    
    fn find_variable(&self, name: &str) -> Option<&VariableInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(var_info) = scope.variables.get(name) {
                return Some(var_info);
            }
        }
        None
    }

    fn find_function(&self, name: &str) -> Option<&FunctionSignature> {
         for scope in self.scopes.iter().rev() { // Functions can also be in outer scopes
            if let Some(func_info) = scope.functions.get(name) {
                return Some(func_info);
            }
        }
        None
    }


    pub fn analyze_program(&mut self, program: &ProgramNode) -> SemanticResult<()> {
        for statement in &program.body {
            self.analyze_statement(statement)?;
        }
        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> SemanticResult<()> {
        match stmt {
            Statement::FunctionDeclaration(func_decl) => {
                // Check for redeclaration in current (global/module) scope
                if self.current_scope_mut().functions.contains_key(&func_decl.name.0) {
                    // For MVP, simple error. Real impl might handle overloading or error differently.
                    return Err(SemanticError::NotImplemented(format!("Function '{}' already declared (overloading not supported in MVP)", func_decl.name.0)));
                }
                
                let mut param_types = Vec::new();
                for p in &func_decl.params {
                    param_types.push(p.ty_annotation); // Assumes type annotations are valid by now
                }

                self.current_scope_mut().functions.insert(
                    func_decl.name.0.clone(),
                    FunctionSignature {
                        param_types,
                        return_type: func_decl.return_type,
                    }
                );

                self.enter_scope();
                let previous_return_type = self.current_function_return_type.replace(func_decl.return_type);

                for param in &func_decl.params {
                    self.current_scope_mut().variables.insert(
                        param.name.0.clone(),
                        VariableInfo { ty: param.ty_annotation, is_const: true } // Params are effectively const
                    );
                }
                self.analyze_block(&func_decl.body)?;

                self.current_function_return_type = previous_return_type;
                self.exit_scope();
                Ok(())
            }
            Statement::VariableDeclaration { name, ty_annotation, initializer, is_const } => {
                let id_name = &name.0;
                let declared_type = ty_annotation.ok_or_else(|| SemanticError::NotImplemented(format!(
                    "Type inference for variable '{}' not yet supported, please provide a type annotation.", id_name
                )))?; // MVP requires type annotations

                if let Some(init_expr) = initializer {
                    let init_type = self.analyze_expression(init_expr)?;
                    if init_type != declared_type {
                        return Err(SemanticError::TypeMismatch {
                            expected_type: declared_type,
                            found_type: init_type,
                            context: format!("initializer for variable '{}'", id_name),
                        });
                    }
                }
                // Check for redeclaration in current scope
                if self.current_scope_mut().variables.contains_key(id_name) {
                     return Err(SemanticError::NotImplemented(format!("Variable '{}' already declared in this scope", id_name)));
                }

                self.current_scope_mut().variables.insert(id_name.clone(), VariableInfo { ty: declared_type, is_const: *is_const });
                Ok(())
            }
            Statement::ExpressionStatement(expr) => {
                self.analyze_expression(expr)?; // Analyze for side effects or errors, type is discarded
                Ok(())
            }
            Statement::ReturnStatement(expr_opt) => {
                let expected_return_type = self.current_function_return_type
                    .ok_or_else(|| SemanticError::NotImplemented("Return statement outside function".to_string()))?;
                
                match expr_opt {
                    Some(expr) => {
                        if expected_return_type == TypeAnnotationNode::Void {
                            return Err(SemanticError::TypeMismatch{
                                expected_type: TypeAnnotationNode::Void, 
                                found_type: self.analyze_expression(expr)?, // Placeholder, analyze to get type
                                context: "return statement in void function".to_string()
                            });
                        }
                        let actual_return_type = self.analyze_expression(expr)?;
                        if actual_return_type != expected_return_type {
                            return Err(SemanticError::TypeMismatch {
                                expected_type: expected_return_type,
                                found_type: actual_return_type,
                                context: "return statement".to_string(),
                            });
                        }
                    }
                    None => { // return;
                        if expected_return_type != TypeAnnotationNode::Void {
                             return Err(SemanticError::TypeMismatch {
                                expected_type: expected_return_type,
                                found_type: TypeAnnotationNode::Void,
                                context: "empty return in non-void function".to_string(),
                            });
                        }
                    }
                }
                Ok(())
            }
            Statement::Empty => Ok(()), // Empty statements are semantically valid (do nothing)
            Statement::Block(block_node) => {
                self.enter_scope();
                self.analyze_block(block_node)?;
                self.exit_scope();
                Ok(())
            }
            // MVP might not implement these fully in semantic analysis yet
            Statement::IfStatement { .. } => Err(SemanticError::NotImplemented("If statement analysis".to_string())),
            Statement::EnergyBlock { .. } => Err(SemanticError::NotImplemented("EnergyBlock analysis".to_string())),
            Statement::ResourceAcquisition { .. } => Err(SemanticError::NotImplemented("ResourceAcquisition analysis".to_string())),
        }
    }

    fn analyze_block(&mut self, block: &BlockNode) -> SemanticResult<()> {
        for stmt in &block.statements {
            self.analyze_statement(stmt)?;
        }
        Ok(())
    }

    fn analyze_expression(&mut self, expr: &ExpressionNode) -> SemanticResult<TypeAnnotationNode> {
        match expr {
            ExpressionNode::Literal(LiteralNode::Number(_)) => Ok(TypeAnnotationNode::Number),
            ExpressionNode::Literal(LiteralNode::String(_)) => Ok(TypeAnnotationNode::String),
            ExpressionNode::Literal(LiteralNode::Boolean(_)) => Ok(TypeAnnotationNode::Boolean),
            ExpressionNode::Identifier(id_node) => {
                if let Some(var_info) = self.find_variable(&id_node.0) {
                    Ok(var_info.ty)
                } else {
                    Err(SemanticError::UndefinedVariable(id_node.0.clone()))
                }
            }
            ExpressionNode::FunctionCall { callee, args } => {
                let signature = self.find_function(&callee.0)
                    .cloned() 
                    .ok_or_else(|| SemanticError::UndefinedFunction(callee.0.clone()))?;

                if args.len() != signature.param_types.len() {
                    return Err(SemanticError::ArgumentCountMismatch {
                        func_name: callee.0.clone(),
                        expected_count: signature.param_types.len(),
                        found_count: args.len(),
                    });
                }

                for (arg_expr, expected_type) in args.iter().zip(signature.param_types.iter()) {
                    let arg_type = self.analyze_expression(arg_expr)?;
                    if arg_type != *expected_type {
                        return Err(SemanticError::TypeMismatch {
                            expected_type: *expected_type,
                            found_type: arg_type,
                            context: format!("argument for function '{}'", callee.0),
                        });
                    }
                }
                Ok(signature.return_type)
            }
            ExpressionNode::BinaryOp { op, left, right } => {
                let left_type = self.analyze_expression(left)?;
                let right_type = self.analyze_expression(right)?;

                // Simplified: For MVP, assume arithmetic ops need numbers and result in number.
                // Comparisons result in boolean.
                match op {
                    BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
                        if left_type == TypeAnnotationNode::Number && right_type == TypeAnnotationNode::Number {
                            Ok(TypeAnnotationNode::Number)
                        } else {
                            Err(SemanticError::TypeMismatch {
                                expected_type: TypeAnnotationNode::Number,
                                found_type: if left_type != TypeAnnotationNode::Number { left_type } else { right_type },
                                context: format!("operand for operator {:?}", op),
                            })
                        }
                    }
                    BinaryOperator::Eq | BinaryOperator::Neq | BinaryOperator::StrictEq | BinaryOperator::StrictNeq |
                    BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Lte | BinaryOperator::Gte => {
                        // For MVP, allow comparison between numbers. Could be extended.
                        if left_type == TypeAnnotationNode::Number && right_type == TypeAnnotationNode::Number {
                             Ok(TypeAnnotationNode::Boolean)
                        } else if left_type == TypeAnnotationNode::String && right_type == TypeAnnotationNode::String {
                             Ok(TypeAnnotationNode::Boolean) // Allow string comparison for equality
                        }
                         else {
                             Err(SemanticError::TypeMismatch {
                                 expected_type: left_type, // Or a more general "comparable types"
                                 found_type: right_type,
                                 context: format!("operands for comparison operator {:?}", op),
                             })
                         }
                    }
                    // LogicalAnd, LogicalOr would require boolean operands and result in boolean.
                    _ => Err(SemanticError::NotImplemented(format!("Binary operator {:?} analysis", op))),
                }
            }
        }
    }
}