use inline_colorization::*;
use std::fs;
use std::process::Command;

const BENCHMARK_RUNS: u16 = 150;
const BENCHMARK_WARMUP: u16 = 10;

struct Prog {
    name: &'static str,
    source: &'static str,
}

const PROGRAMS: &[Prog] = &[
    Prog {
        name: "iter_fib_40_x_200000",
        source: r#"
        function main() {
            for _ in 0..200000 {
                let a = 0;
                let b = 1;
                let c = 0;
                for i in 0..39 {
                    c = a + b;
                    a = b;
                    b = c;
                }
            }
        }"#,
    },
    Prog {
        name: "rec_fib_30",
        source: r#"
        function fib(n) {
            if n <= 1 { return n; }
            return fib(n - 1) + fib(n - 2);
        }

        function main() {
            print(fib(30));
        }"#,
    },
    Prog {
        name: "multiply_branch_modulo_x_1000000",
        source: r#"
        function main() {
            let count = 0;
            let result = 1;
            while count < 1000000 {
                result *= 2;
                if result > 1000000 {
                    result %= 1000000;
                }
                count += 1;
            }
            print(result);
        }"#,
    },
    Prog {
        name: "sqrt_x_10000000",
        source: r#"
        function main() {
            let x = 0.0;
            for i in 0..10000000 {
                x += float(i).sqrt();
            }
            print(x);
        }"#,
    },
    Prog {
        name: "sieve_100000",
        source: r#"
        function main() {
            let limit = 100000;
            let sieve = range(limit);
            sieve[0] = 0;
            sieve[1] = 0;
            let i = 2;
            while i * i <= limit {
                if sieve[i] != 0 {
                    let j = i * i;
                    while j < limit {
                        sieve[j] = 0;
                        j += i;
                    }
                }
                i += 1;
            }
            let count = 0;
            for x in sieve {
                if x != 0 { count += 1; }
            }
            print(count);
        }"#,
    },
    Prog {
        name: "string_ops_array_split_search_x_50000",
        source: r#"
        function main() {
            let s = "the quick brown fox";
            let count = 0;
            for _ in 0..50000 {
                let parts = s.split(" ");
                if parts.contains("fox") {
                    count += 1;
                }
            }
            print(count);
        }"#,
    },
    Prog {
        name: "fizzbuzz_x_1000000",
        source: r#"
        function main() {
            let last = "";
            for i in 1..1000001 {
                if i % 15 == 0 {
                    last = "FizzBuzz";
                } else if i % 3 == 0 {
                    last = "Fizz";
                } else if i % 5 == 0 {
                    last = "Buzz";
                } else {
                    last = str(i);
                }
            }
            print(last);
        }"#,
    },
];

#[cold]
#[inline(never)]
pub fn benchmark() {
    let exe = std::env::current_exe().unwrap_or_else(|e| {
        eprintln!("{color_red}SPOCK ERROR{color_reset}\nCannot locate current executable: {e}");
        std::process::exit(1);
    });
    let temp_dir = std::env::temp_dir().join(format!("spock-bench-{}", std::process::id()));
    fs::create_dir_all(&temp_dir).unwrap_or_else(|e| {
        eprintln!(
            "{color_red}SPOCK ERROR{color_reset}\nCannot create benchmark directory {}: {e}",
            temp_dir.display()
        );
        std::process::exit(1);
    });

    let json_path = temp_dir.join("hyperfine.json");
    let quote = |s: &str| format!("'{}'", s.replace('\'', "'\\''"));
    let exe = quote(&exe.to_string_lossy());
    let mut hyperfine = Command::new("hyperfine");
    hyperfine
        .arg("--warmup")
        .arg(BENCHMARK_WARMUP.to_string())
        .arg("--runs")
        .arg(BENCHMARK_RUNS.to_string())
        .arg("--export-json")
        .arg(&json_path);

    for program in PROGRAMS {
        let path = temp_dir.join(format!("{}.spock", program.name));
        fs::write(&path, program.source).unwrap_or_else(|e| {
            eprintln!(
                "{color_red}SPOCK ERROR{color_reset}\nCannot write benchmark program {}: {e}",
                path.display()
            );
            std::process::exit(1);
        });
        hyperfine
            .arg("--command-name")
            .arg(program.name)
            .arg(format!("{} {}", exe, quote(&path.to_string_lossy())));
    }

    let output = hyperfine.output().unwrap_or_else(|e| {
        eprintln!("{color_red}SPOCK ERROR{color_reset}\nCannot run hyperfine: {e}");
        std::process::exit(1);
    });
    if !output.status.success() {
        eprintln!(
            "{color_red}SPOCK ERROR{color_reset}\nhyperfine failed with status {}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
        std::process::exit(output.status.code().unwrap_or(1));
    }

    let json = fs::read_to_string(&json_path).unwrap_or_else(|e| {
        eprintln!(
            "{color_red}SPOCK ERROR{color_reset}\nCannot read hyperfine results {}: {e}",
            json_path.display()
        );
        std::process::exit(1);
    });

    for chunk in json.split("\"command\"").skip(1) {
        let name = chunk
            .split_once(':')
            .and_then(|(_, rest)| rest.split('"').nth(1))
            .unwrap_or("unknown");
        let mean = chunk
            .split_once("\"mean\"")
            .and_then(|(_, rest)| rest.split_once(':'))
            .map(|(_, rest)| {
                rest.trim_start()
                    .chars()
                    .take_while(|c| c.is_ascii_digit() || matches!(c, '.' | '-' | '+' | 'e' | 'E'))
                    .collect::<String>()
            })
            .and_then(|n| n.parse::<f64>().ok())
            .unwrap_or(0.0)
            * 1000.0;
        println!("{name}: {mean:.3} ms");
    }
}
