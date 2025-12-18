//! Tests for scientific notation control in float serialization

use serde::Serialize;
use serde_saphyr::{to_fmt_writer_with_options, to_string, SerializerOptions};

#[test]
fn very_large_float_default_may_use_scientific_notation() {
    #[derive(Serialize)]
    struct Doc {
        value: f64,
    }

    // Use a number that ryu will format with scientific notation
    let doc = Doc {
        value: 1.23456789012345e20,
    };

    let out = to_string(&doc).unwrap();
    // Default behavior: ryu may use scientific notation for very large numbers
    assert!(
        out.contains('e') || out.contains('E'),
        "Expected scientific notation for very large float by default, got: {}",
        out
    );
}

/// Helper to check if the YAML numeric value contains scientific notation
fn has_scientific_notation(yaml: &str) -> bool {
    // Check for e+, e-, E+, E- patterns that indicate scientific notation
    // This avoids false positives from field names
    yaml.contains("e+") || yaml.contains("e-") || yaml.contains("E+") || yaml.contains("E-")
}

#[test]
fn very_large_float_without_scientific_notation() {
    #[derive(Serialize)]
    struct Doc {
        num: f64,
    }

    // Use a number that ryu would format with scientific notation
    let doc = Doc {
        num: 1.23456789012345e15,
    };

    let opts = SerializerOptions {
        scientific_notation_threshold: None,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Should NOT contain scientific notation
    assert!(
        !has_scientific_notation(&out),
        "Expected no scientific notation, got: {}",
        out
    );
}

#[test]
fn large_integer_float_without_scientific_notation() {
    #[derive(Serialize)]
    struct Doc {
        num: f64,
    }

    let doc = Doc {
        num: 100000000000.0,
    };

    let opts = SerializerOptions {
        scientific_notation_threshold: None,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Should NOT contain scientific notation
    assert!(
        !has_scientific_notation(&out),
        "Expected no scientific notation, got: {}",
        out
    );
    // Should contain the full number
    assert!(
        out.contains("100000000000"),
        "Expected full number, got: {}",
        out
    );
}

#[test]
fn small_float_unchanged() {
    #[derive(Serialize)]
    struct Doc {
        value: f64,
    }

    let doc = Doc { value: 3.14159 };

    let opts = SerializerOptions {
        scientific_notation_threshold: None,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    assert!(
        out.contains("3.14159"),
        "Expected plain decimal, got: {}",
        out
    );
}

#[test]
fn integer_like_float_without_scientific_notation() {
    #[derive(Serialize)]
    struct Doc {
        max_connections: f64,
    }

    let doc = Doc {
        max_connections: 100000000000.0,
    };

    let opts = SerializerOptions {
        scientific_notation_threshold: None,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Should render as the full number with .0 suffix
    assert!(
        out.contains("100000000000.0"),
        "Expected 100000000000.0, got: {}",
        out
    );
}

#[test]
fn negative_large_float_without_scientific_notation() {
    #[derive(Serialize)]
    struct Doc {
        value: f64,
    }

    let doc = Doc {
        value: -50000000000.0,
    };

    let opts = SerializerOptions {
        scientific_notation_threshold: None,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    assert!(
        out.contains("-50000000000"),
        "Expected negative number without scientific notation, got: {}",
        out
    );
}

#[test]
fn special_floats_unchanged() {
    #[derive(Serialize)]
    struct Doc {
        nan: f64,
        inf: f64,
        neg_inf: f64,
    }

    let doc = Doc {
        nan: f64::NAN,
        inf: f64::INFINITY,
        neg_inf: f64::NEG_INFINITY,
    };

    let opts = SerializerOptions {
        scientific_notation_threshold: None,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    assert!(out.contains(".nan"), "Expected .nan, got: {}", out);
    assert!(out.contains(".inf"), "Expected .inf, got: {}", out);
    assert!(out.contains("-.inf"), "Expected -.inf, got: {}", out);
}

#[test]
fn roundtrip_without_scientific_notation() {
    #[derive(Serialize, serde::Deserialize, Debug, PartialEq)]
    struct Doc {
        value: f64,
    }

    let original = Doc {
        value: 100000000000.0,
    };

    let opts = SerializerOptions {
        scientific_notation_threshold: None,
        ..Default::default()
    };

    let mut yaml = String::new();
    to_fmt_writer_with_options(&mut yaml, &original, opts).unwrap();

    let parsed: Doc = serde_saphyr::from_str(&yaml).unwrap();
    assert_eq!(original.value, parsed.value, "Value should roundtrip correctly");
}

