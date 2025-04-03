#[macro_export]
macro_rules! error {
    ($x: expr, $y: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK ERROR:{color_reset}\n{color_bright_red}CONTEXT:{color_reset}\n{style_bold}{}{style_reset}\n{color_bright_red}ERROR:{color_reset}\n{}\n--------------", $x, $y
        );
        std::process::exit(1);
    };
    ($x: expr, $y: expr, $z: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK ERROR:{color_reset}\n{color_bright_red}CONTEXT:{color_reset}\n{style_bold}{}{style_reset}\n{color_bright_red}ERROR:{color_reset}\n{}\n{color_bright_red}POSSIBLE FIX:{color_reset}\n{}\n--------------", $x, $y, $z
        );
        std::process::exit(1);
    }
}

#[macro_export]
macro_rules! error_b {
    ($x: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK ERROR:{color_reset}\n{}\n--------------",
            $x
        );
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! parsing_error {
    ($x: expr) => {
        eprintln!(
            "--------------\n{color_red}SPOCK PARSING ERROR:{color_reset}\n{}\n--------------",
            $x
        );
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! print {
    ($($x:tt)*) => {
        #[cfg(debug_assertions)]
        println!("\x1b[33m[LOG] {}\x1b[0m", format!($($x)*))
    }
}



#[macro_export]
macro_rules! format_lines {
    ($line: expr) => {
        if format!("{}", $line).chars().last().unwrap() != '}' {
            format!("{};\n", $line)
        } else {
            format!("{}\n", $line)
        }
    };
}
