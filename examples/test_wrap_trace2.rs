fn main() {
    let expected_lines = [
        "  test2: externalIPs is a list of IP addresses for which nodes in the cluster will",
        "    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The",
        "    user is responsible for ensuring that traffic arrives at a node with this IP.  A",
        "    common example is external load-balancers that are not part of the Kubernetes",
        "    system.",
    ];
    
    for (i, line) in expected_lines.iter().enumerate() {
        println!("Line {}: len={}", i+1, line.len());
        // Find where double spaces are
        let mut pos = 0;
        for (j, c) in line.char_indices() {
            if c == ' ' && j + 1 < line.len() && line.chars().nth(j+1) == Some(' ') {
                println!("  Double space at position {}", j);
            }
        }
    }
    
    println!("\n\nAnalysis of line 2:");
    let l2 = "    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The";
    // Column just before ".  The"
    let pos = l2.find(".  The").unwrap();
    println!("'.  The' starts at position {} (column {})", pos, pos + 1);
    println!("Length at Kubernetes. (before double space): {}", pos + 10);
    println!("If we broke at position 80, we'd break at: '{}'", &l2[79..std::cmp::min(85, l2.len())]);
}
