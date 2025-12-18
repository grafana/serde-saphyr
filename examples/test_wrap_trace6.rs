fn main() {
    // Key insight: Go yaml.v2 breaks at spaces but only at the FIRST space in a run
    // So for "Kubernetes.  The", there are two spaces after the period
    // Break can only happen at the first space (after "Kubernetes.")
    // If column at that point is <= best_width, no break happens
    // Then "  The" stays on the same line
    
    let s = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    // Go yaml.v2 emitter logic (simplified):
    // - Scan character by character
    // - Track `spaces` bool (true if prev char was space)
    // - At each space: if column > best_width && !spaces: insert break
    
    let wrap_col = 80;
    let key_prefix_len = 9; // "  test2: "
    let cont_indent = 4;
    
    let mut column = 0;
    let mut spaces = true; // Start as if previous was space (no break before first char)
    let mut line_num = 1;
    let mut line_content = String::new();
    
    // Add key prefix to first line
    line_content.push_str("  test2: ");
    column = key_prefix_len;
    
    for c in s.chars() {
        if c == ' ' {
            // Check break condition BEFORE writing the space
            // Go's condition: column > best_width && !spaces
            // But wait - we haven't written the space yet, so column is current position
            // Actually, Go checks AFTER incrementing column in the emitter
            // Let's check: should we break?
            if column > wrap_col && !spaces {
                // Break!
                println!("Line {}: {} (len={})", line_num, line_content, line_content.len());
                line_num += 1;
                line_content = " ".repeat(cont_indent);
                column = cont_indent;
            }
            
            line_content.push(c);
            column += 1;
            spaces = true;
        } else {
            line_content.push(c);
            column += 1;
            spaces = false;
        }
    }
    println!("Line {}: {} (len={})", line_num, line_content, line_content.len());
    
    println!("\n\nExpected:");
    let expected = [
        "  test2: externalIPs is a list of IP addresses for which nodes in the cluster will",
        "    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The",
        "    user is responsible for ensuring that traffic arrives at a node with this IP.  A",
        "    common example is external load-balancers that are not part of the Kubernetes",
        "    system.",
    ];
    for (i, line) in expected.iter().enumerate() {
        println!("Line {}: {} (len={})", i+1, line, line.len());
    }
}
