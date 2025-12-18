//! Tests for YAML quote style behavior matching Go's yaml.v3
//!
//! Go's yaml.v3 quoting rules:
//! - Double quotes by default for strings that need quoting
//! - Single quotes only when the string contains double quotes (to avoid escaping)

use serde::Serialize;
use serde_saphyr::to_string;

#[derive(Serialize)]
struct SimpleValue {
    value: String,
}

/// Simple strings that need quoting (look like booleans/numbers) should use double quotes
#[test]
fn test_simple_strings_use_double_quotes() {
    // Strings that look like booleans
    let v = SimpleValue {
        value: "true".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains(r#"value: "true""#),
        "Expected double quotes for 'true', got: {}",
        yaml
    );

    let v = SimpleValue {
        value: "false".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains(r#"value: "false""#),
        "Expected double quotes for 'false', got: {}",
        yaml
    );

    // Strings that look like numbers
    let v = SimpleValue {
        value: "123".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains(r#"value: "123""#),
        "Expected double quotes for '123', got: {}",
        yaml
    );

    let v = SimpleValue {
        value: "3.14".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains(r#"value: "3.14""#),
        "Expected double quotes for '3.14', got: {}",
        yaml
    );

    // Strings that look like null
    let v = SimpleValue {
        value: "null".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains(r#"value: "null""#),
        "Expected double quotes for 'null', got: {}",
        yaml
    );
}

/// Strings with control characters must use double quotes (for escaping)
#[test]
fn test_strings_with_control_chars_use_double_quotes() {
    let v = SimpleValue {
        value: "line1\nline2".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains(r#"value: "line1\nline2""#),
        "Expected double quotes for string with newline, got: {}",
        yaml
    );

    let v = SimpleValue {
        value: "tab\there".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains(r#"value: "tab\there""#),
        "Expected double quotes for string with tab, got: {}",
        yaml
    );
}

/// Plain strings that don't need quoting should remain unquoted
#[test]
fn test_plain_strings_remain_unquoted() {
    let v = SimpleValue {
        value: "hello".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains("value: hello\n"),
        "Expected unquoted string, got: {}",
        yaml
    );

    let v = SimpleValue {
        value: "simple_value".to_string(),
    };
    let yaml = to_string(&v).unwrap();
    assert!(
        yaml.contains("value: simple_value\n"),
        "Expected unquoted string, got: {}",
        yaml
    );
}
