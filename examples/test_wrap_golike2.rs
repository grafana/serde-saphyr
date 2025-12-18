fn main() {
    // Go yaml.v2 logic (from emitter.go):
    // 1. Write the character
    // 2. Increment column
    // 3. At space: check "if column > best_width && !spaces" -> emit newline + indent
    //    The key is: break happens AFTER writing the space
    //    So space stays on current line, next line starts with the non-space char
    
    let value = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    let first_line_offset = 9; // "  test2: " 
    let cont_indent = 4;
    let wrap_col = 80;
    
    let mut col = first_line_offset;
    let mut line = 1;
    let mut spaces = false;
    let mut line_content = String::from("  test2: ");
    
    for c in value.chars() {
        // Write the character first
        line_content.push(c);
        col += 1;
        
        if c == ' ' {
            // After writing a space, check if we should break
            // Condition: column > best_width AND previous char wasn't a space
            if col > wrap_col && !spaces {
                // Break now - the space stays on this line, newline comes after
                println!("Line {}: {} (len={})", line, line_content, line_content.len());
                line += 1;
                line_content = " ".repeat(cont_indent);
                col = cont_indent;
            }
            spaces = true;
        } else {
            spaces = false;
        }
    }
    println!("Line {}: {} (len={})", line, line_content, line_content.len());
    
    println!("\n\nExpected:");
    println!("Line 1:   test2: externalIPs is a list of IP addresses for which nodes in the cluster will (len=82)");
    println!("Line 2:     also accept traffic for this service.  These IPs are not managed by Kubernetes.  The (len=88)");
    println!("Line 3:     user is responsible for ensuring that traffic arrives at a node with this IP.  A (len=84)");
    println!("Line 4:     common example is external load-balancers that are not part of the Kubernetes (len=81)");
    println!("Line 5:     system. (len=11)");
}
