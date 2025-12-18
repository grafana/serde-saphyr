use serde_saphyr::{ChompIndicator, SerializerOptions, to_fmt_writer_with_options};

/// Test that block_scalar_chomp: Strip forces `|-` and strips trailing newlines
#[test]
fn block_scalar_chomp_strip() {
    let input = "line1\nline2\n"; // Has trailing newline

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        block_scalar_chomp: Some(ChompIndicator::Strip),
        ..Default::default()
    };

    let mut buf = String::new();
    to_fmt_writer_with_options(&mut buf, &input, opts).unwrap();

    // Should produce `|-` (strip indicator)
    assert!(buf.contains("|-"), "Expected strip indicator `|-`, got:\n{buf}");
    assert!(!buf.contains("|+"), "Should not contain keep indicator");

    // When parsed back, the trailing newline should be stripped
    let parsed: String = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(parsed, "line1\nline2", "Strip chomp should remove trailing newlines");
}

/// Test that block_scalar_chomp: Clip forces `|`
#[test]
fn block_scalar_chomp_clip() {
    let input = "line1\nline2"; // No trailing newline

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        block_scalar_chomp: Some(ChompIndicator::Clip),
        ..Default::default()
    };

    let mut buf = String::new();
    to_fmt_writer_with_options(&mut buf, &input, opts).unwrap();

    // Should produce `|` (clip indicator, no `-` or `+`)
    assert!(buf.starts_with("|\n") || buf.starts_with("|2") || buf.contains("\n|"), 
            "Expected clip indicator `|`, got:\n{buf}");
    assert!(!buf.contains("|-"), "Should not contain strip indicator");
    assert!(!buf.contains("|+"), "Should not contain keep indicator");

    // When parsed back, clip adds exactly one trailing newline
    let parsed: String = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(parsed, "line1\nline2\n", "Clip chomp should add one trailing newline");
}

/// Test that block_scalar_chomp: Keep forces `|+`
#[test]
fn block_scalar_chomp_keep() {
    let input = "line1\nline2\n"; // Has one trailing newline

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        block_scalar_chomp: Some(ChompIndicator::Keep),
        ..Default::default()
    };

    let mut buf = String::new();
    to_fmt_writer_with_options(&mut buf, &input, opts).unwrap();

    // Should produce `|+` (keep indicator)
    assert!(buf.contains("|+"), "Expected keep indicator `|+`, got:\n{buf}");
}

/// Test that None (default) auto-detects based on trailing newlines
#[test]
fn block_scalar_chomp_auto() {
    // No trailing newline -> strip
    let input1 = "line1\nline2";
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        block_scalar_chomp: None,
        ..Default::default()
    };
    let mut buf1 = String::new();
    to_fmt_writer_with_options(&mut buf1, &input1, opts).unwrap();
    assert!(buf1.contains("|-"), "No trailing newline should use `|-`, got:\n{buf1}");

    // One trailing newline -> clip
    let input2 = "line1\nline2\n";
    let mut buf2 = String::new();
    to_fmt_writer_with_options(&mut buf2, &input2, opts).unwrap();
    assert!(!buf2.contains("|-") && !buf2.contains("|+"), 
            "One trailing newline should use `|` (clip), got:\n{buf2}");

    // Two trailing newlines -> keep
    let input3 = "line1\nline2\n\n";
    let mut buf3 = String::new();
    to_fmt_writer_with_options(&mut buf3, &input3, opts).unwrap();
    assert!(buf3.contains("|+"), "Two trailing newlines should use `|+`, got:\n{buf3}");
}

/// Test the specific use case from the issue: nested block scalars should consistently use `|-`
#[test]
fn nested_block_scalar_strip_for_go_compat() {
    use serde::Serialize;
    use std::collections::HashMap;

    #[derive(Serialize)]
    struct ConfigMap {
        data: HashMap<String, String>,
    }

    // Inner YAML content that would be serialized as a block scalar
    let inner_yaml = r#"- "simple query"
- |
  sum(
        floor(
          max by (cluster, node) (metric{})
        )
      )
"#;

    let mut data = HashMap::new();
    data.insert("queries.yaml".to_string(), inner_yaml.to_string());
    let cm = ConfigMap { data };

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        block_scalar_chomp: Some(ChompIndicator::Strip),
        ..Default::default()
    };

    let mut buf = String::new();
    to_fmt_writer_with_options(&mut buf, &cm, opts).unwrap();

    // The outer block scalar for queries.yaml should use `|-`
    assert!(buf.contains("|-"), "Expected strip indicator for Go yaml.v3 compat, got:\n{buf}");
}






