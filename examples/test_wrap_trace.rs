fn main() {
    // Exact test string with double spaces
    let s = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    // Simulate Go yaml.v2 line wrapping with column tracking
    let wrap_col = 80;
    let key_prefix = "  test2: "; // 2 spaces indent + "test2: "
    let continuation_indent = 4; // 4 spaces on continuation lines
    
    let words: Vec<&str> = s.split(' ').collect();
    let mut column = key_prefix.len();
    let mut first_line = true;
    
    println!("Go yaml.v2 behavior simulation:");
    println!("wrap_col = {}, key_prefix_len = {}, continuation_indent = {}", wrap_col, key_prefix.len(), continuation_indent);
    println!("");
    
    print!("{}", key_prefix);
    
    for (idx, word) in words.iter().enumerate() {
        // Need to add a space if we're not at line start
        let line_start = if first_line { key_prefix.len() } else { continuation_indent };
        
        if column > line_start {
            print!(" ");
            column += 1;
        }
        
        // Write the word
        print!("{}", word);
        column += word.len();
        
        // Check if we should break after this word
        // Go yaml.v2 checks: column > best_width && !spaces
        // !spaces means: previous character was not a space
        // But with word-based processing, we just check column after the word
        let should_break = idx + 1 < words.len() && !word.is_empty() && column > wrap_col;
        
        if should_break {
            println!(" (len={})", column);
            for _ in 0..continuation_indent {
                print!(" ");
            }
            column = continuation_indent;
            first_line = false;
        }
    }
    println!(" (len={})", column);
    
    println!("\n\nExpected Go yaml.v2 output:");
    println!("  test2: externalIPs is a list of IP addresses for which nodes in the cluster will");
    println!("    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The");
    println!("    user is responsible for ensuring that traffic arrives at a node with this IP.  A");
    println!("    common example is external load-balancers that are not part of the Kubernetes");
    println!("    system.");
}
