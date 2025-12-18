use serde_saphyr::{self, SerializerOptions, to_fmt_writer_with_options};

// Dedicated tests for prefer_block_scalars behavior

#[test]
fn prefer_block_scalars_literal_newlines_and_trailing() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // Case 1: multiline with no trailing newline => use literal |-
    let s1 = "a\nb".to_string();
    let mut out1 = String::new();
    to_fmt_writer_with_options(&mut out1, &s1, opts).unwrap();
    assert_eq!(out1, "|-\n  a\n  b\n");
    let r1: String = serde_saphyr::from_str(&out1).unwrap();
    assert_eq!(s1, r1);

    // Case 2: multiline with two trailing newlines => use |+ and keep one visible empty line
    let s2 = "a\nb\n\n".to_string();
    let mut out2 = String::new();
    to_fmt_writer_with_options(&mut out2, &s2, opts).unwrap();
    // Note: Blank lines are not indented in the current implementation
    let expected2 = "|+\n  a\n  b\n\n";
    assert_eq!(out2, expected2, "Unexpected YAML for 2 trailing newlines: {out2}");
    let r2: String = serde_saphyr::from_str(&out2).unwrap();
    assert_eq!(s2, r2);

    // Case 3: multiline with four trailing newlines => use |+ and keep three visible empty lines
    let s3 = "a\nb\n\n\n\n".to_string();
    let mut out3 = String::new();
    to_fmt_writer_with_options(&mut out3, &s3, opts).unwrap();
    // Note: Blank lines are not indented in the current implementation
    let expected3 = "|+\n  a\n  b\n\n\n\n";
    assert_eq!(out3, expected3, "Unexpected YAML for 4 trailing newlines: {out3}");
    let r3: String = serde_saphyr::from_str(&out3).unwrap();
    assert_eq!(s3, r3);
}

#[test]
fn prefer_block_scalars_no_auto_fold_for_single_line() {
    // Single-line strings should NOT be auto-folded when line_width is None.
    // This matches Go yaml.v3 behavior where Marshal() with default settings
    // does not wrap/fold long single-line strings.
    let long = "word ".repeat(20) + "end"; // > 80 chars

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &long, opts).unwrap();

    // Should output as plain scalar, not folded block scalar
    assert!(!out.starts_with(">"), "Should not auto-fold single-line string, got:\n{out}");
    assert!(out.trim().contains("word word"), "Should contain original string, got:\n{out}");
    
    // Round-trip must preserve the original string
    let back: String = serde_saphyr::from_str(&out).unwrap();
    assert_eq!(back, long);
}
