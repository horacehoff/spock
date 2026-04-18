use crate::Data;
use crate::DynamicLibFn;
use crate::Instr;
use crate::Pools;
use inline_colorization::*;
use smol_str::SmolStr;
use std::hint::black_box;

#[cold]
#[inline(never)]
pub fn benchmark(
    instructions: &[Instr],
    registers: &mut [Data],
    pools: &mut Pools,
    instr_src: &[(Instr, (usize, usize), u16)],
    sources: &[(SmolStr, String)],
    fn_registers: &[Vec<u16>],
    warmup_runs: usize,
    samples_count: usize,
    verbose: bool,
    fn_dyn_libs: &[DynamicLibFn],
    allocated_arg_count: usize,
    allocated_call_depth: usize,
) {
    let mut times_ns: Vec<u128> = Vec::with_capacity(samples_count);

    for _ in 0..warmup_runs {
        black_box(crate::vm::execute(
            black_box(instructions),
            black_box(&mut registers.to_vec()),
            black_box(&mut pools.to_owned()),
            black_box(instr_src),
            black_box(sources),
            black_box(fn_registers),
            black_box(fn_dyn_libs),
            black_box(allocated_arg_count),
            black_box(allocated_call_depth),
        ));
    }
    for _ in 0..samples_count {
        let registers = &mut registers.to_vec();
        let now = std::time::Instant::now();
        black_box(crate::vm::execute(
            black_box(instructions),
            black_box(registers),
            black_box(&mut pools.to_owned()),
            black_box(instr_src),
            black_box(sources),
            black_box(fn_registers),
            black_box(fn_dyn_libs),
            black_box(allocated_arg_count),
            black_box(allocated_call_depth),
        ));
        times_ns.push(now.elapsed().as_nanos());
    }
    if verbose {
        let mut sorted = times_ns.clone();
        sorted.sort_unstable();

        let min_ns = sorted[0];
        let max_ns = *sorted.last().unwrap();
        let median_ns = if samples_count.is_multiple_of(2) {
            (sorted[samples_count / 2 - 1] + sorted[samples_count / 2]) / 2
        } else {
            sorted[samples_count / 2]
        };

        let mean_ns = times_ns.iter().sum::<u128>() as f64 / samples_count as f64;

        let variance = times_ns
            .iter()
            .map(|&x| (x as f64 - mean_ns).powi(2))
            .sum::<f64>()
            / (samples_count as f64);
        let std_deviation = variance.sqrt();

        let std = std_deviation / (samples_count as f64).sqrt();
        let confidence_interval_margin = 1.96 * std;

        let lower_confidence_interval = mean_ns - confidence_interval_margin;
        let upper_confidence_interval = mean_ns + confidence_interval_margin;

        println!(
            "\nBENCHMARK RESULTS\n---{color_blue}Program{color_reset}---\n{color_blue}{}{color_reset}\n-------------",
            sources[0].1
        );
        println!("Samples          : {}", samples_count);
        println!("Min              : {:.3} ms", min_ns as f64 / 1000000.0);
        println!("Max              : {:.3} ms", max_ns as f64 / 1000000.0);
        println!(
            "Median           : {:.3} ms",
            median_ns as f64 / 1_000_000.0
        );
        println!("Mean             : {:.3} ms", mean_ns / 1000000.0);
        println!("Stand. deviation : {:.3} ms", std_deviation / 1000000.0);
        println!(
            "95% Confidence interval for mean  : [{:.3} .. {:.3}] ms",
            lower_confidence_interval / 1000000.0,
            upper_confidence_interval / 1000000.0
        );
        println!(
            "{bg_red}{color_white}Average exec time: {:.3} ms (±{:.3} ms with 95% confidence){color_reset}{bg_reset}",
            mean_ns / 1000000.0,
            confidence_interval_margin / 1000000.0
        );
    }
}
