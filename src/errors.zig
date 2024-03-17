pub const Error = error{
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

    UndefinedVariable,
    AlreadyDeclareType,
    UnknowType,
    NonDeclarationGiven,
    AlreadyDeclareAtRuntime,
};
