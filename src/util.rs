pub fn error(message: &str, tip: &str) {
    eprintln!("--------------\n{}\n{}\n{}\n{}\n--------------","\u{001b}[31mCOMPUTE ERROR:\u{001b}[0m", message, "\u{001b}[34mPOSSIBLE SOLUTION:\u{001b}[0m", tip);
    std::process::exit(1);
}