fn main() {
    // Expected output lines (from golden file)
    let lines = [
        "  test2: externalIPs is a list of IP addresses for which nodes in the cluster will",
        "    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The",
        "    user is responsible for ensuring that traffic arrives at a node with this IP.  A",
        "    common example is external load-balancers that are not part of the Kubernetes",
        "    system.",
    ];
    
    // Reconstruct the original string
    let key = "test2: ";
    let indent = "    "; // 4 spaces
    let first_indent = "  "; // 2 spaces
    
    // The original value (without wrapping)
    let value = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    println!("Original value length: {}", value.len());
    println!("\nExpected line breaks (break happens AFTER these words):");
    
    let mut pos = 0;
    for (i, line) in lines.iter().enumerate() {
        let content = if i == 0 {
            line.strip_prefix(first_indent).unwrap().strip_prefix(key).unwrap()
        } else {
            line.strip_prefix(indent).unwrap()
        };
        
        // Find where this content ends in original value
        let end_pos = pos + content.len();
        println!("Line {}: len={}, content ends with '{}...' at char {}", 
            i+1, 
            line.len(),
            &content[content.len().saturating_sub(20)..],
            end_pos);
        
        // Skip the space(s) between lines
        if end_pos < value.len() {
            let next_char = value.chars().nth(end_pos);
            println!("  Next char after line: {:?}", next_char);
            if next_char == Some(' ') {
                pos = end_pos + 1;
                // Check for double space
                if end_pos + 1 < value.len() && value.chars().nth(end_pos + 1) == Some(' ') {
                    pos = end_pos + 2; // Skip double space by adding to next line
                    println!("  (double space - second space goes to next line)");
                }
            }
        }
        pos = end_pos + 1; // Skip the single space
    }
    
    // What column would trigger a break?
    println!("\n\nColumn analysis:");
    println!("Line 1: 82 chars total, key prefix = 9 chars, value on line = {} chars", 82 - 9);
    println!("Break happens after 'will' at column 82");
    println!("");
    println!("Line 2: 88 chars total, indent = 4 chars");
    println!("Ends with 'The' - why didn't it break after 'Kubernetes.'?");
    
    // Trace word by word
    println!("\n\nWord-by-word trace with wrap_col=80:");
    let words: Vec<&str> = value.split(' ').collect();
    let first_line_offset = 9; // "  test2: " 
    let cont_indent = 4;
    let wrap_col = 80;
    
    let mut col = first_line_offset;
    let mut line = 1;
    
    for (i, word) in words.iter().enumerate() {
        let space = if col == first_line_offset && line == 1 { 0 } else if col == cont_indent { 0 } else { 1 };
        col += space + word.len();
        
        if col > wrap_col && !word.is_empty() {
            println!("After '{}' col={} > {} -> BREAK (line {} ends)", word, col, wrap_col, line);
            line += 1;
            // But wait - we already wrote the word on this line, so we break AFTER it
            col = cont_indent;
        }
    }
}
