fn main() {
    // Test string with double spaces (like the externalIPs description)
    let s = "externalIPs is a list of IP addresses for which nodes in the cluster will also accept traffic for this service.  These IPs are not managed by Kubernetes.  The user is responsible for ensuring that traffic arrives at a node with this IP.  A common example is external load-balancers that are not part of the Kubernetes system.";
    
    println!("String length: {}", s.len());
    println!("String has double spaces: {}", s.contains("  "));
    
    let data = serde_json::json!({
        "test2": s
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
    
    println!("\nActual output:");
    for (i, line) in output.lines().enumerate() {
        println!("{:3}| {} (len={})", i+1, line, line.len());
    }
    
    println!("\nExpected (from Go yaml.v2):");
    let expected = r#"test2: externalIPs is a list of IP addresses for which nodes in the cluster will
  also accept traffic for this service.  These IPs are not managed by Kubernetes.  The
  user is responsible for ensuring that traffic arrives at a node with this IP.  A
  common example is external load-balancers that are not part of the Kubernetes
  system."#;
    for (i, line) in expected.lines().enumerate() {
        println!("{:3}| {} (len={})", i+1, line, line.len());
    }
}
