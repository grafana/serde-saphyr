fn main() {
    // Test different wrap widths to find which one matches expected output
    let value = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    let first_line_offset = 9;
    let cont_indent = 4;
    
    for wrap_col in [80, 85, 90, 100] {
        println!("\n=== wrap_col = {} ===", wrap_col);
        
        let mut col = first_line_offset;
        let mut line = 1;
        let mut spaces = false;
        let mut line_content = String::from("  test2: ");
        
        let chars: Vec<char> = value.chars().collect();
        let mut i = 0;
        
        while i < chars.len() {
            let c = chars[i];
            
            if c == ' ' {
                if col > wrap_col && !spaces {
                    println!("Line {}: {} (len={})", line, line_content, line_content.len());
                    line += 1;
                    line_content = " ".repeat(cont_indent);
                    col = cont_indent;
                    while i < chars.len() && chars[i] == ' ' {
                        i += 1;
                    }
                    continue;
                } else {
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
    }
    
    println!("\n\n=== Expected ===");
    println!("Line 1:   test2: externalIPs is a list of IP addresses for which nodes in the cluster will (len=82)");
    println!("Line 2:     also accept traffic for this service.  These IPs are not managed by Kubernetes.  The (len=88)");
    println!("Line 3:     user is responsible for ensuring that traffic arrives at a node with this IP.  A (len=84)");
    println!("Line 4:     common example is external load-balancers that are not part of the Kubernetes (len=81)");
    println!("Line 5:     system. (len=11)");
}
