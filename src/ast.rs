#[derive(Debug, PartialEq, Clone)]
pub enum ExprOpKind {
    EAdd,
    ESub,
    EMul,
    EDiv,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOpKind {
    UPlus,
    UMinus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableType {
    Int,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentOpKind {
    AEqual,
    AAdd,
    ASub,
    AMul,
    ADiv,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Number(isize),

    Variable(String),

    Expr {
        left: Box<Ast>,
        operator: ExprOpKind,
        right: Box<Ast>,
    },

    Monomial {
        operator: UnaryOpKind,
        expr: Box<Ast>,
    },

    VariableDeclaration {
        name: String,
        data_type: VariableType,
        expr: Box<Ast>,
    },

    VariableAssignment {
        name: String,
        operator: AssignmentOpKind,
        expr: Box<Ast>,
    },

    FunctionCall {
        name: String,
        argument: Box<Ast>,
    },
}
