mod util;
use util::error;


use std::fs;
use std::io::Write;
use fancy_regex::Regex;


fn main() {
    let mut functions: Vec<(&str, &str, Vec<&str>)> = vec![];
    let mut content = fs::read_to_string("foo.txt").unwrap();


    // PRE_PROCESSING


    // Remove comments
    content = Regex::new(r"//.*").unwrap().replace_all(&content, "").parse().unwrap();
    println!("{:?}", content);



    // Register functions
    let function_regex = Regex::new(r"(?ms)^func\s+(\w+)\s*\((.*?)\)\s*\{(.*?)}(?=((\s*func|\z)))").unwrap();
    let function_results: Vec<_> = function_regex.captures_iter(&content).collect();

    let line_regex = Regex::new(r"(?m)^(.*|})(?:;|\s*{|(?<=}))").unwrap();

    for func_match in function_results.iter() {
        let function = func_match.as_ref().unwrap();
        
        // Separate lines by commas
        let line_results: Vec<_> = line_regex.captures_iter(function.get(3).unwrap().as_str().trim()).map(|line| line.as_ref().unwrap().get(0).unwrap().as_str()).collect();
        
        functions.push((function.get(1).unwrap().as_str(), function.get(2).unwrap().as_str(), line_results));
    }

    // Cache functions
    let mut functions_file = fs::File::create(".functions").unwrap();
    functions_file.write_all(format!("{:?}", functions).as_bytes()).unwrap();

    if functions.clone().into_iter().filter(|function| function.0 == "main").collect::<Vec<(&str, &str, Vec<&str>)>>().len() == 0 {
        error("No main function", "Add 'func main() {}' to your file");
    }


    println!("{:?}", functions);
}
