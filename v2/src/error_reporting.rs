pub fn report_error(source: &[u8], message: &str, start: usize) {
    let line_number = get_line_number(source, start);
    println!("Error at line {line_number}! {message}");
    std::process::exit(-1);
}

fn get_line_number(source: &[u8], index: usize) -> usize {
    let mut line_number = 1;
    for char in source.iter().take(index) {
        if *char == b'\n' {
            line_number += 1;
        }
    }

    line_number
}
