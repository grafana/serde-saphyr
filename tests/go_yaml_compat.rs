//! Tests for go-yaml (gopkg.in/yaml.v3) byte-for-byte compatibility.
//!
//! These tests ensure serde-saphyr produces output identical to go-yaml
//! for consistent Kubernetes/Tanka tooling integration.

use serde::Serialize;
use serde_saphyr::SerializerOptions;

// =============================================================================
// Issue 1: Empty Arrays/Sequences
// =============================================================================

#[derive(Serialize)]
struct ContainerWithArrays {
    env: Vec<String>,
    #[serde(rename = "volumeMounts")]
    volume_mounts: Vec<String>,
    volumes: Vec<String>,
}

#[test]
fn test_empty_arrays_as_brackets() {
    let c = ContainerWithArrays {
        env: vec![],
        volume_mounts: vec![],
        volumes: vec![],
    };

    let opts = SerializerOptions {
        empty_array_as_brackets: true,
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &c, opts).unwrap();

    // go-yaml expected output
    let expected = "env: []\nvolumeMounts: []\nvolumes: []\n";
    assert_eq!(buf, expected, "got:\n{}", buf);
}

#[test]
fn test_empty_arrays_default_behavior() {
    let c = ContainerWithArrays {
        env: vec![],
        volume_mounts: vec![],
        volumes: vec![],
    };

    // Default behavior (empty_array_as_brackets = false)
    let yaml = serde_saphyr::to_string(&c).unwrap();

    // Should NOT contain brackets for empty arrays
    assert!(!yaml.contains("[]"), "got:\n{}", yaml);
}

#[test]
fn test_mixed_empty_and_nonempty_arrays() {
    #[derive(Serialize)]
    struct Mixed {
        empty: Vec<String>,
        filled: Vec<String>,
    }

    let m = Mixed {
        empty: vec![],
        filled: vec!["a".to_string(), "b".to_string()],
    };

    let opts = SerializerOptions {
        empty_array_as_brackets: true,
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &m, opts).unwrap();

    let expected = "empty: []\nfilled:\n  - a\n  - b\n";
    assert_eq!(buf, expected, "got:\n{}", buf);
}

// =============================================================================
// Issue 2: String Quoting for Dash-Starting Strings with Special Characters
// =============================================================================

#[derive(Serialize)]
struct Args {
    args: Vec<String>,
}

#[test]
fn test_dash_string_with_quotes_not_quoted() {
    // This exact pattern is used in Prometheus/Mimir configs
    let args = Args {
        args: vec![
            r#"-forward.selectors={__name__="target_info"},{__name__="traces_info"}"#.to_string(),
        ],
    };

    let yaml = serde_saphyr::to_string(&args).unwrap();

    // go-yaml expected output: unquoted value
    let expected_line = r#"- -forward.selectors={__name__="target_info"},{__name__="traces_info"}"#;
    assert!(
        yaml.contains(expected_line),
        "Expected unquoted string, got:\n{}",
        yaml
    );

    // Should NOT be wrapped in quotes
    assert!(
        !yaml.contains("'-forward"),
        "Should not have single quotes:\n{}",
        yaml
    );
}

#[test]
fn test_dash_string_with_quotes_exact() {
    let args = Args {
        args: vec![r#"-forward.selectors={__name__="target_info"}"#.to_string()],
    };

    let yaml = serde_saphyr::to_string(&args).unwrap();

    // Exact byte-for-byte comparison with go-yaml
    let expected = "args:\n  - -forward.selectors={__name__=\"target_info\"}\n";
    assert_eq!(yaml, expected, "got:\n{}", yaml);
}

#[test]
fn test_roundtrip_string_with_embedded_quotes() {
    let original = Args {
        args: vec![
            r#"-forward.selectors={__name__="target_info"},{__name__="traces_info"}"#.to_string(),
        ],
    };

    let yaml = serde_saphyr::to_string(&original).unwrap();

    #[derive(serde::Deserialize, PartialEq, Debug)]
    struct ArgsD {
        args: Vec<String>,
    }

    let parsed: ArgsD = serde_saphyr::from_str(&yaml).unwrap();
    assert_eq!(parsed.args, original.args);
}

// =============================================================================
// Issue 3: Asterisk Quoting Style
// =============================================================================

#[test]
fn test_asterisk_uses_single_quotes() {
    #[derive(Serialize)]
    struct Doc {
        value: String,
    }

    let doc = Doc {
        value: "*".to_string(),
    };

    let yaml = serde_saphyr::to_string(&doc).unwrap();

    // go-yaml uses single quotes for asterisk
    let expected = "value: '*'\n";
    assert_eq!(
        yaml, expected,
        "Expected single-quoted asterisk, got:\n{}",
        yaml
    );
}

#[test]
fn test_asterisk_prefix_uses_single_quotes() {
    // Strings starting with '*' should use single quotes (go-yaml behavior)
    #[derive(Serialize)]
    struct Doc {
        items: Vec<String>,
    }

    let doc = Doc {
        items: vec!["*-suffix".to_string(), "*foo".to_string()],
    };

    let yaml = serde_saphyr::to_string(&doc).unwrap();

    // go-yaml uses single quotes for strings starting with asterisk
    assert!(
        yaml.contains("'*-suffix'"),
        "Expected single-quoted '*-suffix', got:\n{}",
        yaml
    );
    assert!(
        yaml.contains("'*foo'"),
        "Expected single-quoted '*foo', got:\n{}",
        yaml
    );
    // Should NOT use double quotes
    assert!(
        !yaml.contains("\"*-suffix\"") && !yaml.contains("\"*foo\""),
        "Should not use double quotes for asterisk-prefixed strings:\n{}",
        yaml
    );
}

#[test]
fn test_ampersand_prefix_uses_single_quotes() {
    // Strings starting with '&' should also use single quotes (go-yaml behavior)
    #[derive(Serialize)]
    struct Doc {
        value: String,
    }

    let doc = Doc {
        value: "&anchor-like".to_string(),
    };

    let yaml = serde_saphyr::to_string(&doc).unwrap();

    // go-yaml uses single quotes for strings starting with ampersand
    assert!(
        yaml.contains("'&anchor-like'"),
        "Expected single-quoted '&anchor-like', got:\n{}",
        yaml
    );
}

// =============================================================================
// Issue 4: Long String Continuation Line Indentation in Nested Maps
// =============================================================================

#[test]
fn test_nested_map_continuation_indent() {
    // Reproduces the KEDA ScaledObject pattern where a long string value
    // is deeply nested inside a sequence item's map
    #[derive(Serialize)]
    struct Metadata {
        query: String,
        #[serde(rename = "type")]
        trigger_type: String,
    }

    #[derive(Serialize)]
    struct Spec {
        triggers: Vec<Metadata>,
    }

    #[derive(Serialize)]
    struct ScaledObject {
        spec: Spec,
    }

    let doc = ScaledObject {
        spec: Spec {
            triggers: vec![Metadata {
                query: "(1 - (min(kubelet_volume_stats_available_bytes{cluster=\"test-cluster\", namespace=\"test-ns\", persistentvolumeclaim=~\"store-gateway-.*\"}/kubelet_volume_stats_capacity_bytes{cluster=\"test-cluster\",namespace=\"test-ns\", persistentvolumeclaim=~\"store-gateway-.*\"}))) * 100".to_string(),
                trigger_type: "prometheus".to_string(),
            }],
        },
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &doc, opts).unwrap();

    println!("Output:\n{}", buf);

    // Verify structure:
    // - The sequence item starts with `- ` at 4 spaces (under triggers:)
    // - The query key is inline after the dash: `    - query:`
    // - Continuation lines should be at 8 spaces (past the value start position)
    let lines: Vec<&str> = buf.lines().collect();

    // Find the query line and its continuation
    let query_line_idx = lines.iter().position(|l| l.contains("query:")).unwrap();
    let query_line = lines[query_line_idx];

    // query: should be inline after dash at 4 spaces: "    - query:"
    assert!(
        query_line.starts_with("    - query:"),
        "query key should be inline after dash at 4 spaces, got: {:?}",
        query_line
    );

    // Continuation lines (if any) should be at 8 spaces (not 4 spaces)
    // Check the next line after query
    if query_line_idx + 1 < lines.len() {
        let next_line = lines[query_line_idx + 1];
        // If it's a continuation of the query value (not the type: key), it should be indented 8 spaces
        if !next_line.contains("type:") && !next_line.trim().is_empty() {
            let indent = next_line.len() - next_line.trim_start().len();
            assert_eq!(
                indent, 8,
                "Continuation line should be at 8 spaces, got {} spaces: {:?}",
                indent, next_line
            );
        }
    }

    // Verify round-trip
    #[derive(serde::Deserialize, Debug)]
    struct MetadataD {
        query: String,
        #[serde(rename = "type")]
        trigger_type: String,
    }

    #[derive(serde::Deserialize, Debug)]
    struct SpecD {
        triggers: Vec<MetadataD>,
    }

    #[derive(serde::Deserialize, Debug)]
    struct ScaledObjectD {
        spec: SpecD,
    }

    let parsed: ScaledObjectD = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(parsed.spec.triggers[0].query, doc.spec.triggers[0].query);
}

/// Test with an extra nesting level (metadata map inside sequence item)
/// to match the exact KEDA ScaledObject structure
#[test]
fn test_deeply_nested_map_continuation_indent() {
    #[derive(Serialize)]
    struct TriggerMetadata {
        query: String,
    }

    #[derive(Serialize)]
    struct Trigger {
        metadata: TriggerMetadata,
        #[serde(rename = "type")]
        trigger_type: String,
    }

    #[derive(Serialize)]
    struct Spec {
        triggers: Vec<Trigger>,
    }

    #[derive(Serialize)]
    struct ScaledObject {
        spec: Spec,
    }

    let doc = ScaledObject {
        spec: Spec {
            triggers: vec![Trigger {
                metadata: TriggerMetadata {
                    query: "(1 - (min(kubelet_volume_stats_available_bytes{cluster=\"test-cluster\", namespace=\"test-ns\", persistentvolumeclaim=~\"store-gateway-.*\"}/kubelet_volume_stats_capacity_bytes{cluster=\"test-cluster\",namespace=\"test-ns\", persistentvolumeclaim=~\"store-gateway-.*\"}))) * 100".to_string(),
                },
                trigger_type: "prometheus".to_string(),
            }],
        },
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &doc, opts).unwrap();

    println!("Deeply nested output:\n{}", buf);

    // Structure:
    // spec:
    //   triggers:
    //     - metadata:
    //         query: (long value...
    //           continuation at 10 spaces
    //       type: prometheus

    let lines: Vec<&str> = buf.lines().collect();
    let query_line_idx = lines.iter().position(|l| l.contains("query:")).unwrap();
    let query_line = lines[query_line_idx];

    // query: should be at 8 spaces (under metadata: which is inline after dash at 4 spaces)
    assert!(
        query_line.starts_with("        query:"),
        "query key should be at 8 spaces, got: {:?}",
        query_line
    );

    // Check continuation line
    if query_line_idx + 1 < lines.len() {
        let next_line = lines[query_line_idx + 1];
        if !next_line.contains("type:") && !next_line.trim().is_empty() {
            let indent = next_line.len() - next_line.trim_start().len();
            // Continuation should be at 10 spaces (query at 8 + one indent_step of 2)
            assert_eq!(
                indent, 10,
                "Continuation line should be at 10 spaces (query indent 8 + indent_step 2), got {} spaces: {:?}",
                indent, next_line
            );
        }
    }

    // Verify round-trip
    #[derive(serde::Deserialize, Debug)]
    struct TriggerMetadataD {
        query: String,
    }

    #[derive(serde::Deserialize, Debug)]
    struct TriggerD {
        metadata: TriggerMetadataD,
        #[serde(rename = "type")]
        #[allow(dead_code)]
        trigger_type: String,
    }

    #[derive(serde::Deserialize, Debug)]
    struct SpecD {
        triggers: Vec<TriggerD>,
    }

    #[derive(serde::Deserialize, Debug)]
    struct ScaledObjectD {
        spec: SpecD,
    }

    let parsed: ScaledObjectD = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(
        parsed.spec.triggers[0].metadata.query,
        doc.spec.triggers[0].metadata.query
    );
}

// =============================================================================
// Combined tests with both options
// =============================================================================

#[test]
fn test_go_yaml_compat_combined() {
    #[derive(Serialize)]
    struct Pod {
        args: Vec<String>,
        env: Vec<String>,
    }

    let pod = Pod {
        args: vec![r#"-config={key="value"}"#.to_string()],
        env: vec![],
    };

    let opts = SerializerOptions {
        empty_array_as_brackets: true,
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &pod, opts).unwrap();

    // Both features combined
    let expected = "args:\n  - -config={key=\"value\"}\nenv: []\n";
    assert_eq!(buf, expected, "got:\n{}", buf);
}

// =============================================================================
// Block scalar fallback for strings with tabs
// =============================================================================

/// Go yaml.v2 falls back to double-quoted style for strings containing tabs,
/// even when block scalars would otherwise be used for multiline strings.
#[test]
fn test_block_scalar_fallback_for_tabs() {
    use std::collections::HashMap;

    let mut data: HashMap<&str, &str> = HashMap::new();
    data.insert("no-tabs", "Line 1\nLine 2\n");
    data.insert("with-tabs", "Line 1\n\tLine 2\n");

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &data, opts).unwrap();

    // no-tabs should use block scalar
    assert!(
        buf.contains("no-tabs: |"),
        "no-tabs should use block scalar style, got:\n{}",
        buf
    );

    // with-tabs should use double-quoted (escaped), not block scalar
    assert!(
        !buf.contains("with-tabs: |"),
        "with-tabs should NOT use block scalar style, got:\n{}",
        buf
    );
    assert!(
        buf.contains("with-tabs: \"Line 1\\n\\tLine 2\\n\""),
        "with-tabs should use double-quoted style with escape sequences, got:\n{}",
        buf
    );
}

/// Single-line strings with tabs should also use quoted style
#[test]
fn test_single_line_with_tabs_quoted() {
    use std::collections::HashMap;

    let mut data: HashMap<&str, &str> = HashMap::new();
    data.insert("tabbed", "col1\tcol2\tcol3");

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &data, opts).unwrap();    // Should use quoted style for tab character
    assert!(
        buf.contains("tabbed: \"col1\\tcol2\\tcol3\""),
        "single-line with tabs should use double-quoted style, got:\n{}",
        buf
    );
}

/// Long strings with tabs AND newlines should be wrapped at line_width
/// This matches Go's yaml.v2 behavior where quoted strings are wrapped
/// with a continuation indent when they exceed the width limit.
#[test]
fn test_long_quoted_string_with_tabs_and_newlines_wrapped() {
    use std::collections::HashMap;

    // Create a string that has:
    // 1. Tabs (forces double-quoted style - can't use block scalars)
    // 2. Newlines (would normally use block scalars, but tabs prevent that)
    // 3. Long enough to exceed 80 char width
    let long_string = "Line 1\n\tLine 2 with tab\nLine 3 is a longer line that should wrap when serialized\nLine 4\n";

    let mut data: HashMap<&str, &str> = HashMap::new();
    data.insert("config", long_string);

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        line_width: Some(80),
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &data, opts).unwrap();

    println!("Output:\n{}", buf);    // Should NOT use block scalar style (tabs prevent it)
    assert!(
        !buf.contains("|"),
        "should NOT use block scalar style for strings with tabs, got:\n{}",
        buf
    );

    // Should use double-quoted style
    assert!(
        buf.contains("config: \""),
        "should use double-quoted style, got:\n{}",
        buf
    );

    // Key test: Should be wrapped across multiple lines (not all on one line)
    // The output should have continuation lines that are indented
    let lines: Vec<&str> = buf.lines().collect();
    assert!(
        lines.len() > 1,
        "long string should be wrapped across multiple lines, got:\n{}",
        buf
    );

    // Find continuation lines (indented lines that are part of the quoted string)
    let continuation_lines: Vec<&str> = lines
        .iter()
        .skip(1) // Skip the first line with "config:"
        .filter(|line| line.starts_with("  ") && !line.is_empty())
        .copied()
        .collect();

    assert!(
        !continuation_lines.is_empty(),
        "should have indented continuation lines for wrapped string, got:\n{}",
        buf
    );    // Verify round-trip: parsing the output should give back the original string
    let parsed: HashMap<String, String> = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(
        parsed.get("config").unwrap(),
        long_string,
        "round-trip should preserve the original string"
    );
}

/// Leading spaces on continuation lines in double-quoted strings should be escaped as `\ `
/// This matches Go's yaml.v2 behavior. Without escaping, YAML line folding would trim
/// the leading spaces.
#[test]
fn test_double_quoted_wrap_escapes_leading_spaces() {
    use std::collections::HashMap;

    // String that REQUIRES double-quoting (contains \t) with double spaces positioned
    // so the wrap occurs right before them. The \t forces double-quote style.
    // Using tab forces the wrapping code path (strings with \n only use block scalars).
    // With line_width=13 and key overhead, first_line_limit ~= 4, so "text." (5 chars)
    // will trigger a wrap, and the continuation starts with the extra space.
    let string_with_double_space = "text.  y and\ttab";

    let mut data: HashMap<&str, &str> = HashMap::new();
    data.insert("config", string_with_double_space);

    let opts = SerializerOptions {
        line_width: Some(13), // Force wrapping after "text." so continuation starts with space
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &data, opts).unwrap();

    println!("Output:\n{}", buf);

    // The continuation line should have `\ ` (escaped space) at start of content
    // This is the Go yaml.v2 behavior for preserving leading spaces
    assert!(
        buf.contains("\\ "),
        "continuation line should escape leading space as '\\ ', got:\n{}",
        buf
    );

    // Verify round-trip: parsing should give back the original string with both spaces
    let parsed: HashMap<String, String> = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(
        parsed.get("config").unwrap(),
        string_with_double_space,
        "round-trip should preserve the double space in the original string"
    );
}

/// Triple (or more) consecutive leading spaces: only the FIRST should be escaped (Go yaml.v2 compat)
#[test]
fn test_double_quoted_wrap_escapes_only_first_leading_space() {
    use std::collections::HashMap;

    // String with triple spaces that requires double quoting (contains \t)
    // With narrow line_width, wrap happens after "text." so continuation starts with spaces
    let string_with_triple_space = "text.   y and\ttab";

    let mut data: HashMap<&str, &str> = HashMap::new();
    data.insert("config", string_with_triple_space);    let opts = SerializerOptions {
        line_width: Some(13), // Force wrapping after "text."
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &data, opts).unwrap();

    println!("Output:\n{}", buf);    // The line break provides 1 space (via YAML line folding), so for 3 original spaces:
    // - fold: 1 space
    // - escaped space `\ `: 1 space
    // - regular space: 1 space
    // = 3 total
    // So we expect `\  ` (backslash + 2 spaces), NOT `\ \ ` (multiple escapes)
    assert!(
        buf.contains("\\  "), // backslash + 2 spaces (fold provides the 3rd)
        "continuation line should escape only first leading space as '\\ ' followed by regular spaces, got:\n{}",
        buf
    );
    assert!(
        !buf.contains("\\ \\ "),
        "should NOT escape multiple leading spaces as '\\ \\ ', got:\n{}",
        buf
    );

    // Verify round-trip preserves all three spaces
    let parsed: HashMap<String, String> = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(
        parsed.get("config").unwrap(),
        string_with_triple_space,
        "round-trip should preserve the triple space in the original string"
    );
}

/// Go yaml.v2 breaks lines BEFORE whitespace sequences, not in the middle.
/// This ensures no trailing spaces on lines - spaces stay with the content they precede.
#[test]
fn test_double_quoted_wrap_no_trailing_spaces() {
    use std::collections::HashMap;

    // String with spaces before a comment character, requiring double quoting (contains \t)
    // When wrapping, the spaces before "#" should stay with "#", not become trailing spaces
    let string_with_indent = "text\t    #comment";

    let mut data: HashMap<&str, &str> = HashMap::new();
    data.insert("k", string_with_indent);

    let opts = SerializerOptions {
        line_width: Some(10), // Force wrapping
        ..Default::default()
    };
    let mut buf = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut buf, &data, opts).unwrap();

    println!("Output:\n{}", buf);    // Check that no line ends with trailing spaces (before the newline)
    for line in buf.lines() {
        // Skip checking the last line which has the closing quote
        if line.ends_with('"') {
            continue;
        }
        assert!(
            !line.ends_with(' '),
            "line should not have trailing spaces: {:?}\nfull output:\n{}",
            line,
            buf
        );
    }

    // Verify round-trip
    let parsed: HashMap<String, String> = serde_saphyr::from_str(&buf).unwrap();
    assert_eq!(
        parsed.get("k").unwrap(),
        string_with_indent,
        "round-trip should preserve the original string"
    );
}