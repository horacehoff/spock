use crate::ArrayPool;
use crate::Data;
use crate::Expr;
use crate::Instr;
use crate::StringPool;
use crate::type_system::DataType;
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

pub struct ParserData<'a> {
    pub registers: *mut Vec<Data>,
    pub fns: *mut Vec<Function>,
    pub pools: *mut Pools,
    pub block_id: u16,
    pub src: (&'a str, &'a str),
    pub instr_src: *mut Vec<(Instr, (usize, usize), u16)>,
    /// Indicates if the current expression being compiled is part of a recursive function
    pub is_parsing_recursive: bool,
    /// Each element is specific to a callsite and indicates the registers that need to be "saved" before a recursive call
    pub fn_registers: *mut Vec<Vec<u16>>,
    pub parsing_fn_id: Option<u16>,
    pub dyn_libs: *mut Vec<Dynamiclib>,
    /// Used for Vec::with_capacity()
    pub allocated_arg_count: *mut usize,
    /// Used for Vec::with_capacity()
    pub allocated_call_depth: *mut usize,
    pub const_registers: *mut Vec<u16>,
    pub free_registers: *mut Vec<u16>,
    /// All source files: index 0 = main file
    /// Vec<(name, content)>
    pub sources: *mut Vec<(SmolStr, String)>,
    /// Index of the file currently being compiled
    pub current_src_file: u16,
}
impl ParserData<'_> {
    pub fn destructure(
        &self,
    ) -> (
        &mut Vec<Data>,
        &mut Vec<Function>,
        &mut Pools,
        &mut Vec<(Instr, (usize, usize), u16)>,
        &mut Vec<Vec<u16>>,
        u16,
        (&str, &str),
        bool,
        Option<u16>,
        &mut Vec<Dynamiclib>,
        &mut usize,
        &mut usize,
        &mut Vec<u16>,
        &mut Vec<u16>,
        &mut Vec<(SmolStr, String)>,
        u16,
    ) {
        (
            unsafe { &mut *self.registers },
            unsafe { &mut *self.fns },
            unsafe { &mut *self.pools },
            unsafe { &mut *self.instr_src },
            unsafe { &mut *self.fn_registers },
            self.block_id,
            self.src,
            self.is_parsing_recursive,
            self.parsing_fn_id,
            unsafe { &mut *self.dyn_libs },
            unsafe { &mut *self.allocated_arg_count },
            unsafe { &mut *self.allocated_call_depth },
            unsafe { &mut *self.const_registers },
            unsafe { &mut *self.free_registers },
            unsafe { &mut *self.sources },
            self.current_src_file,
        )
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: SmolStr,
    pub register_id: u16,
    pub infered_type: DataType,
}
