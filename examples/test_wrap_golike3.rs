fn main() {
    // Key realization: When Go breaks at a space, it:
    // 1. Does NOT write trailing spaces before the break
    // 2. Does NOT write leading spaces after the break (except indentation)
    // 
    // So when we encounter a space and decide to break:
    // - We've written up to the word before the space
    // - We emit newline + indent
    // - The space is consumed (not written)
    // - Continue with next char
    
    let value = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    let first_line_offset = 9; // "  test2: " 
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
        
        if c == ' ' {
            // Check if we should break at this space
            if col > wrap_col && !spaces {
                // Break: don't write the space, emit newline + indent
                println!("Line {}: {} (len={})", line, line_content, line_content.len());
                line += 1;
                line_content = " ".repeat(cont_indent);
                col = cont_indent;
                // Skip all spaces in the run
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
