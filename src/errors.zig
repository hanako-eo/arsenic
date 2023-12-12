pub const Error = error {
    AllocationOutOfMemory,

    InvalidNumberFormat,
    IllegalChar,
    IllegalUtf8Char,
    MissingChar,

    InvalidExport,
    NonModuleExport,
    InvalidOptionExpression,
    UnauthorisedOperation,
    UnauthorisedLitteral,
    OptionalChaining,

    AlreadyDeclareType,
    UnknowType,
    NonDeclarationGiven,
    AlreadyDeclareVariable,
    AlreadyDeclareFunction,
};
