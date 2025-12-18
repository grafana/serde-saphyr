fn main() {
    // Go yaml.v2's exact condition for breaking at a space:
    // allow_breaks && !spaces && column > best_width && !is_space(value, i+1)
    //
    // This means: break at a space if:
    // 1. Previous char was NOT a space (so we're at the START of a space run)
    // 2. Column already exceeds best_width
    // 3. NEXT char is NOT a space (so we're at a single space, or at the END of a space run)
    
    let value = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    let first_line_offset = 9;
    let cont_indent = 4;
    let wrap_col = 80;
    
    let mut col = first_line_offset;
    let mut line = 1;
    let mut spaces = false;
    let mut line_content = String::from("  test2: ");
    
    let chars: Vec<char> = value.chars().collect();
    let mut i = 0;
    
    while i < chars.len() {
        let c = chars[i];
        let next_is_space = i + 1 < chars.len() && chars[i + 1] == ' ';
        
        if c == ' ' {
            // Go's condition: break if !spaces && column > best_width && !is_space(next)
            if !spaces && col > wrap_col && !next_is_space {
                // Break: don't write the space, emit newline + indent
                println!("Line {}: {} (len={})", line, line_content, line_content.len());
                line += 1;
                line_content = " ".repeat(cont_indent);
                col = cont_indent;
                // Skip all spaces in the run (consume them)
                while i < chars.len() && chars[i] == ' ' {
                    i += 1;
                }
                continue;
            } else {
                // No break, write the space
                line_content.push(c);
                col += 1;
                spaces = true;
            }
        } else {
            line_content.push(c);
            col += 1;
            spaces = false;
        }
        i += 1;
    }
    println!("Line {}: {} (len={})", line, line_content, line_content.len());
    
    println!("\n\nExpected:");
    println!("Line 1:   test2: externalIPs is a list of IP addresses for which nodes in the cluster will (len=82)");
    println!("Line 2:     also accept traffic for this service.  These IPs are not managed by Kubernetes.  The (len=88)");
    println!("Line 3:     user is responsible for ensuring that traffic arrives at a node with this IP.  A (len=84)");
    println!("Line 4:     common example is external load-balancers that are not part of the Kubernetes (len=81)");
    println!("Line 5:     system. (len=11)");
}
