use crate::Data;
use crate::Expr;
use crate::Instr;
use crate::type_system::DataType;
use crate::vm::ArrayPool;
use crate::vm::StringPool;
use libffi::middle::Type;
use libloading::Library;
use smol_str::SmolStr;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: SmolStr,
    pub args: Box<[SmolStr]>,
    pub code: Box<[Expr]>,
    pub impls: Vec<FunctionImpl>,
    pub is_recursive: bool,
    pub id: u16,
    pub returns_void: bool,
    /// Index into the sources table for error reporting
    pub src_file: u16,
}

#[derive(Debug, Clone)]
pub struct FunctionImpl {
    pub loc: u16,
    pub args_loc: Box<[u16]>,
    pub arg_types: Box<[DataType]>,
    pub return_type: DataType,
}

#[derive(Debug)]
pub struct FnSignature {
    pub name: SmolStr,
    pub args: Box<[DataType]>,
    pub return_type: DataType,
    pub id: u16,
}

#[derive(Debug)]
pub struct Dynamiclib {
    pub name: SmolStr,
    pub fns: Box<[FnSignature]>,
}

#[derive(Debug)]
pub struct DynamicLibFn {
    pub arg_types: Box<[Type]>,
    pub return_type: Type,
    pub lib: Library,
    pub ptr: libffi::middle::CodePtr,
    pub cif: libffi::middle::Cif,
}

#[derive(Clone)]
pub struct Pools {
    pub array_pool: ArrayPool,
    pub string_pool: StringPool,
}

#[derive(Clone, Copy)]
pub struct Ctx<'src> {
    pub block_id: u16,
    pub src: (&'src str, &'src str),
    pub is_parsing_recursive: bool,
    pub parsing_fn_id: Option<u16>,
    pub current_src_file: u16,
}

/// Shared mutable compilation state. Passed as `&mut State` — no unsafe needed.
pub struct State<'a> {
    pub registers: &'a mut Vec<Data>,
    pub fns: &'a mut Vec<Function>,
    pub pools: &'a mut Pools,
    pub instr_src: &'a mut Vec<(Instr, (usize, usize), u16)>,
    pub fn_registers: &'a mut Vec<Vec<u16>>,
    pub dyn_libs: &'a mut Vec<Dynamiclib>,
    pub allocated_arg_count: &'a mut usize,
    pub allocated_call_depth: &'a mut usize,
    pub const_registers: &'a mut Vec<u16>,
    pub free_registers: &'a mut Vec<u16>,
    pub sources: &'a mut Vec<(SmolStr, String)>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: SmolStr,
    pub register_id: u16,
    pub infered_type: DataType,
}
