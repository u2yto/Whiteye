// 値
#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(isize),
    Float(f32),
}

#[derive(Debug, PartialEq)]
pub enum ValueType {
    Integer,
    Float,
}

// 演算子
#[derive(Debug, PartialEq)]
pub enum ExprOpKind {
    EAdd,
    ESub,
    EMul,
    EDiv,
}

// 単項演算子
#[derive(Debug, PartialEq)]
pub enum UnaryOpKind {
    UPlus,
    UMinus,
}

// 代入演算子
#[derive(Debug, PartialEq)]
pub enum AssignmentOpKind {
    AEqual,
    AAdd,
    ASub,
    AMul,
    ADiv,
}

// 抽象構文木
#[derive(Debug, PartialEq)]
pub enum Ast {
    Literal(Value), // 値

    Variable(String), // 変数

    // 多項式
    Expr {
        left: Box<Ast>,
        operator: ExprOpKind,
        right: Box<Ast>,
    },

    // 単項式
    Monomial {
        operator: UnaryOpKind,
        expr: Box<Ast>,
    },

    // 変数宣言
    VariableDeclaration {
        name: String,
        value_type: ValueType,
        expr: Box<Ast>,
    },

    // 変数への代入
    VariableAssignment {
        name: String,
        operator: AssignmentOpKind,
        expr: Box<Ast>,
    },

    // 組み込み関数の呼び出し
    FunctionCall {
        name: String,
        argument: Box<Ast>,
    },
}
