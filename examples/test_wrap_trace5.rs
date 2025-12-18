fn main() {
    // Full test string  
    let s = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    // The expected output shows:
    // Line 2 ends with "Kubernetes.  The" (88 chars total)
    // This means Go yaml.v2 didn't break after "Kubernetes." even though col > 80
    // Why? Because at the space after "Kubernetes.", prev_was_space = false
    // So "column > 80 && !spaces" would be TRUE and should break
    // UNLESS... Go uses a different check
    
    // Let me look at actual expected output character-by-character
    let expected = [
        "  test2: externalIPs is a list of IP addresses for which nodes in the cluster will",
        "    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The",
        "    user is responsible for ensuring that traffic arrives at a node with this IP.  A",
        "    common example is external load-balancers that are not part of the Kubernetes",
        "    system.",
    ];
    
    // Concatenate expected lines (removing indents) to see word boundaries
    for (i, line) in expected.iter().enumerate() {
        let content = line.trim_start();
        println!("Line {}: {} chars, content starts with: '{}'", 
            i+1, 
            line.len(),
            &content[..std::cmp::min(20, content.len())]);
    }
    
    // Key observation: Line 2 has 88 chars but should have broken
    // UNLESS Go yaml.v2's best_width is not strictly 80
    // Or the algorithm is: break BEFORE the word that would exceed, not after
    
    println!("\n\nActual experiment: what's the column at each word boundary?");
    let words: Vec<&str> = s.split(' ').collect();
    let key_prefix = 9; // "  test2: " length
    let cont_indent = 4;
    
    let mut column = key_prefix;
    let mut line = 1;
    
    for (i, word) in words.iter().enumerate() {
        if i > 0 && !word.is_empty() {
            // Previous was space, add 1
            column += 1;
        } else if i > 0 {
            // Empty word (from double space), still add space
            column += 1;
        }
        let word_end_col = column + word.len();
        if word_end_col > 85 && !word.is_empty() {
            println!("Line {} ends at col {} before word '{}' (would be {})", 
                line, column-1, word, word_end_col);
            line += 1;
            column = cont_indent + word.len();
        } else {
            column = word_end_col;
        }
    }
    println!("Line {} ends at col {}", line, column);
}
