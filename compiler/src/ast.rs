// src/ast.rs

#[derive(Debug, PartialEq, Clone, Copy)] // Added Copy
pub enum TypeAnnotationNode {
    Number,
    String,
    Boolean,
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierNode(pub String);

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralNode {
    Number(i64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Mod,
    Eq, Neq, StrictEq, StrictNeq,
    Lt, Gt, Lte, Gte,
    LogicalAnd, LogicalOr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionNode {
    Literal(LiteralNode),
    Identifier(IdentifierNode),
    BinaryOp {
        op: BinaryOperator,
        left: Box<ExpressionNode>,
        right: Box<ExpressionNode>,
    },
    FunctionCall {
        callee: IdentifierNode,
        args: Vec<ExpressionNode>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameterNode {
    pub name: IdentifierNode,
    pub ty_annotation: TypeAnnotationNode,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockNode {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclarationNode {
    pub name: IdentifierNode,
    pub params: Vec<FunctionParameterNode>,
    pub return_type: TypeAnnotationNode,
    pub body: BlockNode,
}

// Placeholders for Energy/Resource related AST nodes (from previous discussions)
// These are not fully handled by the MVP parser/codegen but are here for structure.
#[derive(Debug, PartialEq, Clone)]
pub enum EnergyConstraint { Limit(String) }
#[derive(Debug, PartialEq, Clone)]
pub enum PowerMode { Low, High }
#[derive(Debug, PartialEq, Clone)]
pub enum EnergyAnnotation {
    Bound(EnergyConstraint),
    BatchIO { batch_size: usize },
    PowerMode(PowerMode),
    Custom(String),
}
#[derive(Debug, PartialEq, Clone)]
pub enum ResourceType {
    CPU { cores: u32, frequency: u32 },
    Memory { size: usize },
    IO { device: String, mode: PowerMode },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    FunctionDeclaration(FunctionDeclarationNode),
    VariableDeclaration {
        is_const: bool,
        name: IdentifierNode,
        ty_annotation: Option<TypeAnnotationNode>,
        initializer: Option<ExpressionNode>,
    },
    ExpressionStatement(ExpressionNode),
    ReturnStatement(Option<ExpressionNode>),
    IfStatement { // Basic structure for future
        condition: Box<ExpressionNode>,
        then_block: BlockNode,
        else_block: Option<BlockNode>,
    },
    Block(BlockNode), // For explicit blocks not tied to functions/ifs
    Empty, // For empty statements (;)
    EnergyBlock { // Placeholder
        annotations: Vec<EnergyAnnotation>,
        body: BlockNode,
    },
    ResourceAcquisition { // Placeholder
        resource: ResourceType,
        constraints: Vec<EnergyConstraint>,
        body: BlockNode,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProgramNode {
    pub body: Vec<Statement>,
}