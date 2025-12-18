fn main() {
    let l2 = "    also accept traffic for this service.  These IPs are not managed by Kubernetes.  The";
    println!("Line 2 length: {}", l2.len());
    
    // Find the double spaces
    for i in 0..l2.len()-1 {
        if &l2[i..i+2] == "  " {
            println!("Double space at column {} (0-indexed), between '{}' and '{}'",
                i, 
                if i > 0 { &l2[i-1..i] } else { "" },
                if i+2 < l2.len() { &l2[i+2..i+3] } else { "" });
        }
    }
    
    // What's at column 80?
    println!("\nAt column 80 (1-indexed): '{}'", &l2[79..80]);
    println!("Columns 75-85: '{}'", &l2[74..std::cmp::min(85, l2.len())]);
    
    // The break decision at column 80 would be:
    // We're at "Kubernetes."(ending at col 83) followed by two spaces
    // After writing "Kubernetes." we're at col 83
    // If !spaces (prev char wasn't space) and column > 80, we'd break
    // But the prev char IS '.' not space... 
    // Wait - Go checks at space positions!
    
    println!("\n\nGo yaml.v2 logic trace:");
    println!("Go scans character by character");
    println!("At each SPACE, it checks: column > best_width && !spaces");
    println!("'spaces' = true if previous character was also a space");
    println!("");
    println!("So at the second space in '.  ' (double space after 'Kubernetes.'):");
    println!("- Previous char was a space");
    println!("- spaces = true");
    println!("- So condition '!spaces' is FALSE");
    println!("- Therefore: don't break!");
    println!("");
    println!("This means Go yaml.v2 never breaks at the second space in a run of spaces.");
}
