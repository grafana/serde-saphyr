//! Tests for automatic line wrapping of long strings (matches Go's yaml.v3 SetWidth behavior)

use serde::{Deserialize, Serialize};
use serde_saphyr::{to_fmt_writer_with_options, to_string, SerializerOptions};

#[test]
fn line_width_wraps_long_strings() {
    #[derive(Serialize, Deserialize)]
    struct Doc {
        description: String,
    }

    let long_text = "This is a very long description that should be wrapped at 80 characters to improve readability and maintainability of the YAML file.";
    let doc = Doc {
        description: long_text.to_string(),
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Should use folded block scalar style
    assert!(
        out.contains(">"),
        "Expected folded block scalar, got:\n{}",
        out
    );

    // All lines should be within 80 characters
    for line in out.lines() {
        assert!(
            line.len() <= 80,
            "Line exceeds 80 characters: {:?} (len={})",
            line,
            line.len()
        );
    }

    // Should still parse back correctly
    let parsed: Doc = serde_saphyr::from_str(&out).unwrap();
    // Note: folded style replaces newlines with spaces, so the parsed value
    // may have slightly different whitespace
    assert!(
        parsed.description.contains("This is a very long description"),
        "Parsed content mismatch"
    );
}

#[test]
fn line_width_none_does_not_wrap() {
    #[derive(Serialize)]
    struct Doc {
        description: String,
    }

    let long_text = "This is a very long description that would normally be wrapped at 80 characters.";
    let doc = Doc {
        description: long_text.to_string(),
    };

    // Default: line_width is None
    let out = to_string(&doc).unwrap();

    // Should NOT use folded block scalar style
    assert!(
        !out.contains(">\n"),
        "Should not use folded block scalar by default, got:\n{}",
        out
    );
}

#[test]
fn line_width_short_strings_not_wrapped() {
    #[derive(Serialize)]
    struct Doc {
        name: String,
    }

    let doc = Doc {
        name: "short".to_string(),
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Short string should not be wrapped
    assert!(
        !out.contains(">\n"),
        "Short strings should not be wrapped, got:\n{}",
        out
    );
    assert!(out.contains("name: short"), "Expected plain scalar");
}

#[test]
fn line_width_with_nested_structure() {
    #[derive(Serialize)]
    struct Outer {
        inner: Inner,
    }

    #[derive(Serialize)]
    struct Inner {
        description: String,
    }

    let long_text = "This is a very long description that should be wrapped at 80 characters to improve readability.";
    let doc = Outer {
        inner: Inner {
            description: long_text.to_string(),
        },
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // All lines should be within 80 characters even with nesting
    for line in out.lines() {
        assert!(
            line.len() <= 80,
            "Line exceeds 80 characters: {:?} (len={})",
            line,
            line.len()
        );
    }
}

#[test]
fn line_width_custom_value() {
    #[derive(Serialize)]
    struct Doc {
        text: String,
    }

    // A string that's longer than 40 chars but shorter than 80
    let text = "This string is about fifty characters long.";
    let doc = Doc {
        text: text.to_string(),
    };

    // With line_width=40, this should wrap
    let opts = SerializerOptions {
        line_width: Some(40),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Should use folded block scalar with width=40
    assert!(
        out.contains(">\n"),
        "Expected folded block scalar with line_width=40, got:\n{}",
        out
    );

    // All lines should be within 40 characters
    for line in out.lines() {
        assert!(
            line.len() <= 40,
            "Line exceeds 40 characters: {:?} (len={})",
            line,
            line.len()
        );
    }
}

#[test]
fn line_width_preserves_multiline_strings() {
    #[derive(Serialize)]
    struct Doc {
        text: String,
    }

    // Multiline strings should still work with prefer_block_scalars
    let doc = Doc {
        text: "line1\nline2\nline3".to_string(),
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        prefer_block_scalars: true,
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Should use literal block scalar for multiline
    assert!(
        out.contains("|-") || out.contains("|"),
        "Expected literal block scalar for multiline, got:\n{}",
        out
    );
}

#[test]
fn line_width_flow_style_not_wrapped() {
    use serde_saphyr::FlowSeq;

    #[derive(Serialize)]
    struct Doc {
        items: FlowSeq<Vec<String>>,
    }

    let long_text =
        "This is a very long string that would normally be wrapped but not in flow style.";
    let doc = Doc {
        items: FlowSeq(vec![long_text.to_string()]),
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // Flow style should not use block scalars
    assert!(
        out.contains("["),
        "Expected flow style sequence, got:\n{}",
        out
    );
    assert!(
        !out.contains(">\n"),
        "Flow style should not use folded block scalars"
    );
}

#[test]
fn line_width_with_array_items() {
    #[derive(Serialize)]
    struct Doc {
        items: Vec<String>,
    }

    let long_text = "This is a very long description that should be wrapped at 80 characters.";
    let doc = Doc {
        items: vec![long_text.to_string(), long_text.to_string()],
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    // All lines should be within 80 characters
    for line in out.lines() {
        assert!(
            line.len() <= 80,
            "Line exceeds 80 characters: {:?} (len={})",
            line,
            line.len()
        );
    }
}

#[test]
fn line_width_roundtrip() {
    #[derive(Serialize, serde::Deserialize, Debug, PartialEq)]
    struct Doc {
        description: String,
    }

    let original_text = "This is a very long description that will be wrapped when serialized but should deserialize back to the original content when parsed.";
    let doc = Doc {
        description: original_text.to_string(),
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut yaml = String::new();
    to_fmt_writer_with_options(&mut yaml, &doc, opts).unwrap();

    // Parse it back
    let parsed: Doc = serde_saphyr::from_str(&yaml).unwrap();

    // Folded style normalizes whitespace, so we compare word content
    let original_words: Vec<&str> = original_text.split_whitespace().collect();
    let parsed_words: Vec<&str> = parsed.description.split_whitespace().collect();
    assert_eq!(
        original_words, parsed_words,
        "Content should be preserved after roundtrip"
    );
}

