use serde_json::json;

fn main() {
    let description = "Automatic connection preference. Set to true for 'ACCEPT_AUTOMATIC' or false for 'ACCEPT_MANUAL'";
    
    println!("Testing description string:");
    println!("  Value: {:?}", description);
    println!("  Length: {} chars", description.len());
    println!("  Starts with quote: {}", description.starts_with('\''));
    println!("  Ends with quote: {}", description.ends_with('\''));
    println!("  Contains quote: {}", description.contains('\''));
    println!("  First char: {:?}", description.chars().next());
    println!("  Starts with letter: {}", description.chars().next().map_or(false, |c| c.is_ascii_alphabetic()));
    
    // Test YAML serialization with line_width
    let data = json!({
        "description": description
    });
    
    let options = serde_saphyr::SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: Some(1000000),
        quote_ambiguous_keys: true,
        quote_numeric_strings: true,
        ..Default::default()
    };
    
    let mut output = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut output, &data, options).unwrap();
    
    println!("\nYAML output with line_width=80:");
    println!("{}", output);
    
    // Also test without line_width
    let options_no_wrap = serde_saphyr::SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: None,
        scientific_notation_threshold: Some(1000000),
        quote_ambiguous_keys: true,
        quote_numeric_strings: true,
        ..Default::default()
    };
    
    let mut output2 = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut output2, &data, options_no_wrap).unwrap();
    
    println!("\nYAML output without line_width:");
    println!("{}", output2);
}
