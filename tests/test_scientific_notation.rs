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


/// Serialize one value the way tk's exporter does — scientific notation above a
/// million and below a ten-thousandth — and return just the scalar.
fn go_style<T: Serialize>(value: T) -> String {
    #[derive(Serialize)]
    struct Doc<T> {
        v: T,
    }

    let opts = SerializerOptions {
        scientific_notation_threshold: Some(1_000_000),
        scientific_notation_small_threshold: Some(0.0001),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &Doc { v: value }, opts).unwrap();
    out.trim_end().strip_prefix("v: ").unwrap().to_string()
}

/// Every expectation here is what Go's `strconv.FormatFloat(v, 'g', -1, 64)`
/// produces, which is what `gopkg.in/yaml.v2` writes and therefore what Tanka
/// exports. They were taken from real `tk export` output.
#[test]
fn scientific_notation_matches_go_exactly() {
    // Exponents past 22, where `10f64.powi(exp)` stops being exact.
    assert_eq!(go_style(1e100_f64), "1e+100");
    assert_eq!(go_style(1e-100_f64), "1e-100");
    assert_eq!(go_style(1e21_f64), "1e+21");

    // 2^53, the last integer float64 counts to in ones. Dividing to find the
    // mantissa rounds this up by one.
    assert_eq!(go_style(9007199254740992_f64), "9.007199254740992e+15");

    // The threshold itself, and just past it.
    assert_eq!(go_style(1000000_f64), "1e+06");
    assert_eq!(go_style(1000001_f64), "1.000001e+06");

    // Sizes that turn up in real manifests.
    assert_eq!(go_style(1048576_f64), "1.048576e+06");
    assert_eq!(go_style(1073741824_f64), "1.073741824e+09");
    assert_eq!(go_style(1500000000_f64), "1.5e+09");

    // Below the small threshold.
    assert_eq!(go_style(0.00001_f64), "1e-05");

    // Signs are kept, on the mantissa and the exponent both.
    assert_eq!(go_style(-1073741824_f64), "-1.073741824e+09");
    assert_eq!(go_style(-1e-100_f64), "-1e-100");
}

/// Integers take a different path in the serializer, and must agree with it.
#[test]
fn scientific_notation_for_integers_matches_go() {
    assert_eq!(go_style(1000000_u64), "1e+06");
    assert_eq!(go_style(1073741824_u64), "1.073741824e+09");
    assert_eq!(go_style(9007199254740992_u64), "9.007199254740992e+15");
    // Larger than float64 counts in ones, so it lands on a neighbour — as it
    // does in Go, which parses JSON numbers into float64 too.
    assert_eq!(go_style(9223372036854775807_u64), "9.223372036854776e+18");

    assert_eq!(go_style(1000000_i64), "1e+06");
    assert_eq!(go_style(-1073741824_i64), "-1.073741824e+09");

    // Below the threshold, integers stay integers.
    assert_eq!(go_style(999999_u64), "999999");
    assert_eq!(go_style(-999999_i64), "-999999");
}

/// Whatever is emitted has to read back as the same number.
#[test]
fn scientific_notation_round_trips() {
    for value in [
        1e100_f64,
        1e-100,
        9007199254740992.0,
        1073741824.0,
        1.000001e6,
        0.00001,
        -1e-100,
        f64::MAX,
        f64::MIN_POSITIVE,
    ] {
        let text = go_style(value);
        let parsed: f64 = text.parse().unwrap_or_else(|e| panic!("{text:?}: {e}"));
        assert_eq!(parsed, value, "{text} did not read back as {value}");
    }
}
