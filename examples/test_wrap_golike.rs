fn main() {
    // Simulate Go yaml.v2 char-by-char with correct space handling
    // Go's condition is: column > best_width && !spaces
    // "spaces" = true means we're in a run of spaces
    // This prevents breaking at the 2nd, 3rd, etc space in a run
    
    let value = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    let first_line_offset = 9; // "  test2: " 
    let cont_indent = 4;
    let wrap_col = 80;
    
    let mut col = first_line_offset;
    let mut line = 1;
    let mut spaces = false; // Was the previous char a space?
    let mut line_content = String::from("  test2: ");
    
    for c in value.chars() {
        if c == ' ' {
            // At a space: check if we should break
            // Break condition: column > best_width AND we're not in a run of spaces
            if col > wrap_col && !spaces {
                // Break here
                println!("Line {}: {} (len={})", line, line_content, line_content.len());
                line += 1;
                line_content = " ".repeat(cont_indent);
                col = cont_indent;
            }
            
            line_content.push(c);
            col += 1;
            spaces = true;
        } else {
            line_content.push(c);
            col += 1;
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
