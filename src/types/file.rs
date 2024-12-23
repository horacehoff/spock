use smartstring::alias::String;

#[macro_export]
macro_rules! file_props {
    ($filepath: expr, $args:expr, $x: expr, $output: expr) => {
        match $x.as_str() {
            "read" => {
                assert_args_number!("read", $args.len(), 0);
                let filecontent = fs::read_to_string($filepath)
                    .expect(error_msg!(format!("Failed to read {}", $filepath)));
                $output = Types::String(filecontent.parse().unwrap())
            }
            "write" => {
                assert_args_number!("write", $args.len(), 1);
                if let Types::String(filecontent) = &$args[0] {
                    let mut f = fs::OpenOptions::new()
                        .write(true)
                        .truncate(true)
                        .open($filepath)
                        .expect(error_msg!(format!("Failed to open {}", $filepath)));
                    f.write_all(filecontent.as_ref()).expect(error_msg!(format!(
                        "Failed to write {} to {}",
                        filecontent, $filepath
                    )));
                    f.flush().unwrap();
                } else {
                    error(
                        &format!("Invalid file content: {:?}", get_printable_form(&$args[0])),
                        "",
                    );
                }
            }
            "append" => {
                assert_args_number!("append", $args.len(), 1);
                if let Types::String(filecontent) = &$args[0] {
                    let mut f = fs::OpenOptions::new()
                        .write(true)
                        .append(true)
                        .open($filepath)
                        .expect(error_msg!(format!("Failed to open {}", $filepath)));
                    f.write_all(filecontent.as_ref()).expect(error_msg!(format!(
                        "Failed to append {} to {}",
                        filecontent, $filepath
                    )));
                    f.flush().unwrap();
                } else {
                    error(
                        &format!("Invalid file content: {:?}", get_printable_form(&$args[0])),
                        "",
                    );
                }
            }
            _ => error(&format!("Unknown function '{}' for object File", $x), ""),
        }
    };
}
