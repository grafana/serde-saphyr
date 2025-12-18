fn main() {
    // Full test string  
    let s = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    // Simulate Go yaml.v2 character-by-character scanning with correct logic
    let wrap_col = 80usize;
    let key_prefix = "  test2: "; // length 9
    let continuation_indent = 4;
    
    let mut column = key_prefix.len();
    let mut first_line = true;
    let mut output = String::new();
    let mut line_output = String::from(key_prefix);
    let mut prev_was_space = false;
    
    for c in s.chars() {
        if c == ' ' {
            // At a space position, check if we should break
            // Condition: column > best_width AND previous char wasn't a space
            if column > wrap_col && !prev_was_space {
                // Break here - write current line and start new one
                println!("{} (len={})", line_output, line_output.len());
                line_output = " ".repeat(continuation_indent);
                column = continuation_indent;
                first_line = false;
            }
            
            // Write the space
            line_output.push(c);
            column += 1;
            prev_was_space = true;
        } else {
            // Write non-space char
            line_output.push(c);
            column += 1;
            prev_was_space = false;
        }
    }
    println!("{} (len={})", line_output, line_output.len());
    
    println!("\n\nExpected:");
    println!("  test2: externalIPs is a list of IP addresses for which nodes in the cluster will (82)");
    println!("    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The (88)");
    println!("    user is responsible for ensuring that traffic arrives at a node with this IP.  A (84)");
    println!("    common example is external load-balancers that are not part of the Kubernetes (81)");
    println!("    system. (11)");
}
