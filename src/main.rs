use crate::array_gc::alloc_array;
use crate::data::Data;
use crate::data::FALSE;
use crate::data::NULL;
use crate::display::format_data;
use crate::errors::ErrType;
use crate::errors::throw_error;
use crate::instr::Instr;
use crate::instr::LibFunc;
use crate::parser::parse;
use crate::parser_data::DynamicLibFn;
use crate::parser_data::Pools;
use crate::util::likely;
use inline_colorization::*;
use mimalloc::MiMalloc;
use parser::*;
use std::fs;
use std::time::Instant;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[path = "./vm/array_gc.rs"]
mod array_gc;
#[path = "./benchmark.rs"]
mod benchmark;
#[path = "./data.rs"]
mod data;
#[path = "./util/display.rs"]
mod display;
#[path = "./util/errors.rs"]
mod errors;
#[path = "./parser/functions/functions.rs"]
mod functions;
#[path = "./instr.rs"]
mod instr;
#[path = "./parser/functions/method_calls.rs"]
mod method_calls;
#[path = "./parser/optimizations.rs"]
mod optimizations;
#[path = "./parser/parser.rs"]
mod parser;
#[path = "./parser/parser_data.rs"]
mod parser_data;
#[path = "./vm/string_gc.rs"]
mod string_gc;
#[path = "./tests.rs"]
#[cfg(test)]
mod tests;
#[path = "./type_system.rs"]
mod type_system;
#[path = "./util/util.rs"]
mod util;
#[path = "./vm/vm.rs"]
mod vm;

/// Live long and prosper
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let debug = args.iter().any(|x| x == "--debug");

    #[cfg(debug_assertions)]
    let filename = "test.spock";

    #[cfg(not(debug_assertions))]
    let filename = &args.get(1).unwrap_or_else(|| {
        println!("{}", util::SPOCK_LOGO);
        std::process::exit(0);
    });

    let contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!(
            "--------------\n{color_red}SPOCK RUNTIME ERROR:{color_reset}\nCannot read {color_bright_red}{style_bold}{filename}{style_reset}{color_reset}\n--------------",
        );
        std::process::exit(1);
    });

    if !debug {
        let (
            instructions,
            mut registers,
            mut arrays,
            instr_src,
            fn_registers,
            fn_dyn_libs,
            allocated_arg_count,
            allocated_call_depth,
            sources,
        ) = parse(&contents, filename, debug);
        vm::execute(
            &instructions,
            &mut registers,
            &mut arrays,
            &instr_src,
            &sources,
            &fn_registers,
            &fn_dyn_libs,
            allocated_arg_count,
            allocated_call_depth,
        );
        return;
    }

    let now = Instant::now();

    let (
        instructions,
        mut registers,
        mut arrays,
        instr_src,
        fn_registers,
        fn_dyn_libs,
        allocated_arg_count,
        allocated_call_depth,
        sources,
    ) = parse(&contents, filename, true);

    println!("COMPILATION TIME: {:.2?}", now.elapsed());
    if args.len() > 2 && args.iter().any(|x| x == "--bench") {
        crate::benchmark::benchmark(
            &instructions,
            &mut registers,
            &mut arrays,
            &instr_src,
            &sources,
            &fn_registers,
            10,
            150,
            args.len() > 3 && args.iter().any(|x| x == "--verbose"),
            &fn_dyn_libs,
            allocated_arg_count,
            allocated_call_depth,
        );
    } else {
        let now = Instant::now();
        vm::execute(
            &instructions,
            &mut registers,
            &mut arrays,
            &instr_src,
            &sources,
            &fn_registers,
            &fn_dyn_libs,
            allocated_arg_count,
            allocated_call_depth,
        );
        println!(
            "EXECUTION TIME: {:.3}ms",
            now.elapsed().as_nanos() / 1000000
        );
    }
}
