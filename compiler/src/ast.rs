// src/ast.rs

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeAnnotationNode {
    Number,
    String,
    Boolean,
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierNode(pub String); // Renomeado de Identifier

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralNode { // Renomeado de Literal
    Number(i64), // Para MVP, 'number' é i64 (representando i32 no backend)
    String(String), // String limpa, sem aspas
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Mod,
    Eq, Neq, StrictEq, StrictNeq,
    Lt, Gt, Lte, Gte,
    LogicalAnd, LogicalOr, // Para futura implementação com short-circuiting
}

// Adicionado para Energy/Resource, mesmo que stubs no codegen MVP
#[derive(Debug, PartialEq, Clone)]
pub enum EnergyConstraint { Limit(String) } // Exemplo
#[derive(Debug, PartialEq, Clone)]
pub enum PowerMode { Low, High } // Exemplo
#[derive(Debug, PartialEq, Clone)]
pub enum EnergyAnnotation {
    Bound(EnergyConstraint),
    BatchIO { batch_size: usize },
    PowerMode(PowerMode),
    Custom(String), // Para extensibilidade
}
#[derive(Debug, PartialEq, Clone)]
pub enum ResourceType {
    CPU { cores: u32, frequency: u32 },
    Memory { size: usize },
    IO { device: String, mode: PowerMode },
}


#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionNode { // Renomeado de Expression
    Literal(LiteralNode),
    Identifier(IdentifierNode),
    BinaryOp {
        op: BinaryOperator,
        left: Box<ExpressionNode>,
        right: Box<ExpressionNode>,
    },
    FunctionCall {
        callee: IdentifierNode, // Para MVP, callee é um IdentifierNode simples
        args: Vec<ExpressionNode>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameterNode { // Renomeado de FunctionParameter
    pub name: IdentifierNode,
    pub ty_annotation: TypeAnnotationNode,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockNode { // Renomeado de Block
    pub statements: Vec<Statement>, // Usa o tipo Statement corrigido
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclarationNode { // Renomeado de FunctionDeclaration
    pub name: IdentifierNode,
    pub params: Vec<FunctionParameterNode>,
    pub return_type: TypeAnnotationNode,
    pub body: BlockNode,
}

// CORRIGIDO: Renomeado de StatementNode para Statement
// E garantindo que FunctionDeclaration seja uma variante aqui.
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    FunctionDeclaration(FunctionDeclarationNode), // Funções podem ser declaradas em nível superior
    VariableDeclaration {
        is_const: bool, // true for const, false for let
        name: IdentifierNode,
        ty_annotation: Option<TypeAnnotationNode>,
        initializer: Option<ExpressionNode>,
    },
    ExpressionStatement(ExpressionNode),
    ReturnStatement(Option<ExpressionNode>),
    // Para if/else, loops, etc., adicionar aqui posteriormente
    IfStatement { // Exemplo, pode ser adiado do MVP inicial se o parser ficar complexo
        condition: Box<ExpressionNode>,
        then_block: BlockNode,
        else_block: Option<BlockNode>,
    },
    Block(BlockNode), // Para blocos explícitos: { ... }
    EnergyBlock { // Mantido da AST original, mesmo que o codegen seja stub
        annotations: Vec<EnergyAnnotation>,
        body: BlockNode,
    },
    ResourceAcquisition { // Mantido da AST original
        resource: ResourceType,
        constraints: Vec<EnergyConstraint>, // Ou um tipo específico de constraint
        body: BlockNode,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProgramNode { // Renomeado de Program
    pub body: Vec<Statement>, // O corpo do programa é uma lista de Statements
}