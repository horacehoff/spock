mod process;
mod util;
mod types;
mod operations;

use std::fs::File;
use std::io::{BufReader, prelude::*};


fn main() {
    let file = File::open("foo.txt").unwrap();
    let reader = BufReader::new(file);
    
    let mut i = 1;
    for line in reader.lines() {
        process::process_line(line.unwrap(), i);
        i += 1;
    }
}
