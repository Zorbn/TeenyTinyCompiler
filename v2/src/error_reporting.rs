pub fn get_line_number(source: &[u8], index: usize) -> usize {
    let mut line_number = 1;
    for char in source.iter().take(index) {
        if *char == b'\n' {
            line_number += 1;
        }
    }

    line_number
}