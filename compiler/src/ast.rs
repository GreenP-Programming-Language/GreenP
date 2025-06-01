#[derive(Debug, PartialEq, Clone)]
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
    Number(i64), // Para MVP, 'number' será i64 (representando i32)
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Mod,
    StrictEq, StrictNeq, Eq, Neq, // Incluindo === e ==
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
        // Para MVP, chamadas são Identificadores simples (e.g. printString)
        callee: IdentifierNode, // Poderia ser Box<ExpressionNode> para chamadas mais complexas
        args: Vec<ExpressionNode>,
    },
    // Futuro: MemberAccess { object: Box<ExpressionNode>, property: IdentifierNode }, // Para console.log
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementNode {
    VariableDeclaration {
        is_const: bool, // true for const, false for let
        name: IdentifierNode,
        ty_annotation: Option<TypeAnnotationNode>, // TypeScript permite inferência
        initializer: Option<ExpressionNode>,
    },
    ExpressionStatement(ExpressionNode), // e.g. function_call();
    ReturnStatement(Option<ExpressionNode>),
    IfStatement {
        condition: Box<ExpressionNode>,
        then_block: BlockNode,
        else_block: Option<BlockNode>, // Option<Box<StatementNode>> if else if is a direct node
    },
    Block(BlockNode), // Para blocos `{ ... }` que não são corpos de função diretamente
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockNode {
    pub statements: Vec<StatementNode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameterNode {
    pub name: IdentifierNode,
    pub ty_annotation: TypeAnnotationNode,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclarationNode {
    pub name: IdentifierNode,
    pub params: Vec<FunctionParameterNode>,
    pub return_type: TypeAnnotationNode, // Em TS, : void é explícito
    pub body: BlockNode,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProgramNode {
    pub body: Vec<StatementNode>, // Programas TS são listas de statements (incluindo func decls)
}