fn main() {
    // Expected line 2: "    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The"
    // Length: 88
    // Starts with 4 spaces of indent
    
    let line2_content = "also accept traffic for this service.  These IPs are not managed by Kubernetes.  The";
    println!("Line 2 content length: {}", line2_content.len());
    
    // Trace column by column
    let cont_indent = 4;
    let mut col = cont_indent;
    let mut prev_was_space = false;
    
    for (i, c) in line2_content.char_indices() {
        col += 1;
        if c == ' ' {
            println!("Space at char {}: col={}, prev_was_space={}", i, col, prev_was_space);
            if col > 80 && !prev_was_space {
                println!("  -> Would break here!");
            }
            prev_was_space = true;
        } else {
            prev_was_space = false;
        }
    }
    
    // The key insight: "Kubernetes." ends at column 79+4=83
    // The space after it is at column 84
    // 84 > 80, so we'd break... but expected DOESN'T break
    // Why?
    
    println!("\n\nColumn trace from start of line 2:");
    let cont_indent = 4;
    col = cont_indent;
    let mut spaces = false;
    
    for c in line2_content.chars() {
        if c == ' ' {
            col += 1;
            if col > 80 && !spaces {
                println!("BREAK: col={} > 80, after char before this space", col);
                break;
            }
            spaces = true;
        } else {
            col += 1;
            spaces = false;
        }
    }
    
    println!("\nBut expected line 2 is 88 chars, which means col goes to 88");
    println!("So Go must NOT break at position 84...");
    println!("\nPossible reasons:");
    println!("1. Go uses a different wrap_col (not 80)");
    println!("2. Go's condition is different (>= vs >, or +1 somewhere)");
    println!("3. The spaces flag works differently");
}
