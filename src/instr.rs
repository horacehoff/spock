#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(u8)]
pub enum Instr {
    Print(u16),

    // LOGIC
    /// Jumps x instructions forwards
    Jmp(u16),
    /// Jumps x instructions backwards
    JmpBack(u16),
    /// Cmp(condition_register_id, jump_size) - jumps if false
    IsFalseJmp(u16, u16),
    InfFloatJmp(u16, u16, u16),
    InfIntJmp(u16, u16, u16),
    InfEqFloatJmp(u16, u16, u16),
    InfEqIntJmp(u16, u16, u16),
    SupFloatJmp(u16, u16, u16),
    SupIntJmp(u16, u16, u16),
    /// Cmp(condition_register_id, jump_size) - jumps if false
    SupEqFloatJmp(u16, u16, u16),
    SupEqIntJmp(u16, u16, u16),
    EqJmp(u16, u16, u16),
    NotEqJmp(u16, u16, u16),
    ArrayEqJmp(u16, u16, u16),
    ArrayNotEqJmp(u16, u16, u16),

    Mov(u16, u16),

    // OPS
    /// (3) = (1) + (2)
    AddFloat(u16, u16, u16),
    AddInt(u16, u16, u16),
    /// (3) = (1) ∪ (2)
    AddArray(u16, u16, u16),
    /// (3) = (1) + (2)
    AddStr(u16, u16, u16),
    /// (3) = (1) * (2)
    MulFloat(u16, u16, u16),
    MulInt(u16, u16, u16),
    SubFloat(u16, u16, u16),
    SubInt(u16, u16, u16),
    DivFloat(u16, u16, u16),
    DivInt(u16, u16, u16),
    ModFloat(u16, u16, u16),
    ModInt(u16, u16, u16),
    PowFloat(u16, u16, u16),
    PowInt(u16, u16, u16),
    Eq(u16, u16, u16),
    NotEq(u16, u16, u16),
    ArrayEq(u16, u16, u16),
    ArrayNotEq(u16, u16, u16),
    SupFloat(u16, u16, u16),
    SupInt(u16, u16, u16),
    SupEqFloat(u16, u16, u16),
    SupEqInt(u16, u16, u16),
    InfFloat(u16, u16, u16),
    InfInt(u16, u16, u16),
    InfEqFloat(u16, u16, u16),
    InfEqInt(u16, u16, u16),
    BoolAnd(u16, u16, u16),
    BoolOr(u16, u16, u16),
    NegFloat(u16, u16),
    NegInt(u16, u16),

    /// CallFunc(n,y) - Jumps to the nth instruction, and adds y as a slot to be set by the Return instruction; VoidReturn/Return will jump back to this location
    CallFunc(u16, u16),
    /// CallFuncRecursive(n, register_id) - Jumps to the nth instruction, register_id is only used during parsing.
    CallFuncRecursive(u16, u16),
    /// Jumps to the instruction right after the last CallFunc encountered by the interpreter
    VoidReturn,
    /// Return(n) => returns the data located in register n
    Return(u16),
    /// RecursiveReturn(n,function_id) => returns the data located in register n, and restores the function's register state
    RecursiveReturn(u16, u16),
    /// SaveFrame(function_location, return_register, function_id)
    SaveFrame(u16, u16, u16),

    StoreFuncArg(u16),
    /// CallLibFunc(function, tgt. register id, dest. register id)
    CallLibFunc(LibFunc, u16, u16),

    // General functions
    Float(u16, u16),
    Int(u16, u16),
    Str(u16, u16),
    Bool(u16, u16),
    Input(u16, u16),
    Floor(u16, u16),
    SqrtFloat(u16, u16),
    /// start,end,dest
    Range(u16, u16, u16),
    // path - dest - create?
    IoOpen(u16, u16, u16),
    IoDelete(u16),

    ArrayMov(u16, u16, u16),
    // different than ArrayMov => looks into the registers
    ArrayMod(u16, u16, u16),
    StrMod(u16, u16, u16),
    ArrayGet(u16, u16, u16),
    ArrayStrGet(u16, u16, u16),

    TheAnswer(u16),
    // array - element
    Push(u16, u16),
    // array - index
    Remove(u16, u16),
    // array/str - dest
    Len(u16, u16),
    // tgt - separator - dest
    Split(u16, u16, u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LibFunc {
    Uppercase = 0,
    Lowercase = 1,
    Contains = 2,
    Trim = 3,
    TrimSequence = 4,
    Index = 5,
    IsNum = 6,
    TrimLeft = 7,
    TrimRight = 8,
    TrimSequenceLeft = 9,
    TrimSequenceRight = 10,
    RIndex = 11,
    Repeat = 12,
    Round = 13,
    Abs = 14,
    ReadFile = 15,
    WriteFile = 16,
    Reverse = 17,
}
