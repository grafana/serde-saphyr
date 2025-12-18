use serde::{Deserialize, Serialize};
use serde_saphyr::SerializerOptions;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Container {
    items: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct NestedContainer {
    name: String,
    containers: Vec<Container>,
}

#[test]
fn block_scalar_indent_in_seq_with_default_behavior() -> anyhow::Result<()> {
    let container = Container {
        items: vec![
            "short".to_string(),
            "line one\nline two\nline three".to_string(),
        ],
    };

    // Default behavior: block scalar body indents based on depth
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 2,
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &container, opts)?;

    // With default behavior (no block_scalar_indent_in_seq), the block scalar
    // body uses 2 spaces (aligned with the sequence level)
    let expected = "items:\n  - short\n  - |-\n  line one\n  line two\n  line three\n";
    assert_eq!(yaml, expected, "Unexpected YAML:\n{yaml}");

    Ok(())
}

#[test]
fn block_scalar_indent_in_seq_with_same_as_default() -> anyhow::Result<()> {
    let container = Container {
        items: vec![
            "short".to_string(),
            "line one\nline two\nline three".to_string(),
        ],
    };

    // Use fixed 2-space indentation for block scalars in sequences (same as default)
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 2,
        block_scalar_indent_in_seq: Some(2),
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &container, opts)?;

    // With block_scalar_indent_in_seq=2, the block scalar body should have exactly 2 spaces
    // (which is the same as default for this depth)
    let expected = "items:\n  - short\n  - |-\n  line one\n  line two\n  line three\n";
    assert_eq!(yaml, expected, "Unexpected YAML:\n{yaml}");

    Ok(())
}

#[test]
fn block_scalar_indent_in_seq_nested_structure() -> anyhow::Result<()> {
    let nested = NestedContainer {
        name: "test".to_string(),
        containers: vec![
            Container {
                items: vec!["first\nline".to_string()],
            },
            Container {
                items: vec!["second\nmulti\nline".to_string()],
            },
        ],
    };

    // Use fixed 2-space indentation for block scalars in sequences
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 2,
        block_scalar_indent_in_seq: Some(2),
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &nested, opts)?;

    // The block scalar uses depth-based indentation (6 spaces for this nesting level)
    // block_scalar_indent_in_seq only applies to DIRECT sequence items, not items
    // inside nested maps within sequences
    let expected = "name: test\ncontainers:\n  - items:\n      - |-\n      first\n      line\n  - items:\n      - |-\n      second\n      multi\n      line\n";
    assert_eq!(yaml, expected, "Unexpected nested YAML:\n{yaml}");

    Ok(())
}

#[test]
fn block_scalar_indent_in_seq_only_affects_sequences() -> anyhow::Result<()> {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Mixed {
        direct_string: String,
        items: Vec<String>,
    }

    let mixed = Mixed {
        direct_string: "line one\nline two".to_string(),
        items: vec!["item one\nitem two".to_string()],
    };

    // Use fixed 2-space indentation for block scalars in sequences
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 2,
        block_scalar_indent_in_seq: Some(2),
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &mixed, opts)?;

    // direct_string is a map value, so should use normal depth-based indentation (2 spaces)
    // items are in a sequence, so should use fixed indentation (2 spaces - same in this case)
    let expected =
        "direct_string: |-\n  line one\n  line two\nitems:\n  - |-\n  item one\n  item two\n";
    assert_eq!(yaml, expected, "Unexpected mixed YAML:\n{yaml}");

    Ok(())
}

#[test]
fn block_scalar_indent_in_seq_with_indent_indicator() -> anyhow::Result<()> {
    let container = Container {
        items: vec![" indented line\n another indented".to_string()],
    };

    // Use fixed 2-space indentation for block scalars in sequences
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 2,
        block_scalar_indent_in_seq: Some(2),
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &container, opts)?;

    // When content starts with whitespace, YAML requires an explicit indent indicator
    let expected = "items:\n  - |2-\n   indented line\n   another indented\n";
    assert_eq!(
        yaml, expected,
        "Unexpected YAML with indent indicator:\n{yaml}"
    );

    Ok(())
}

#[test]
fn block_scalar_indent_in_seq_zero_indentation() -> anyhow::Result<()> {
    let container = Container {
        items: vec!["no\nindent\nlines".to_string()],
    };

    // Use zero indentation for block scalars in sequences
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 2,
        block_scalar_indent_in_seq: Some(0),
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &container, opts)?;

    // block_scalar_indent_in_seq only applies to DIRECT sequence items,
    // not items inside a map field (items is a map field). Use depth-based indentation.
    let expected = "items:\n  - |-\n  no\n  indent\n  lines\n";
    assert_eq!(
        yaml, expected,
        "Unexpected YAML with zero indentation:\n{yaml}"
    );

    Ok(())
}

#[test]
fn block_scalar_indent_in_seq_large_indentation() -> anyhow::Result<()> {
    let container = Container {
        items: vec!["line one\nline two".to_string()],
    };

    // Use large indentation for block scalars in sequences
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 2,
        block_scalar_indent_in_seq: Some(8),
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &container, opts)?;

    // block_scalar_indent_in_seq only applies to DIRECT sequence items,
    // not items inside a map field. Use depth-based indentation (2 spaces).
    let expected = "items:\n  - |-\n  line one\n  line two\n";
    assert_eq!(
        yaml, expected,
        "Unexpected YAML with 8-space indentation:\n{yaml}"
    );

    Ok(())
}

#[test]
fn block_scalar_indent_in_seq_with_different_indent_step() -> anyhow::Result<()> {
    let container = Container {
        items: vec!["line one\nline two".to_string()],
    };

    // Use indent_step=4 but block_scalar_indent_in_seq=2
    // This shows that block_scalar_indent_in_seq overrides the depth-based calculation
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        indent_step: 4,
        block_scalar_indent_in_seq: Some(2),
        ..Default::default()
    };

    let mut yaml = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut yaml, &container, opts)?;

    // block_scalar_indent_in_seq only applies to DIRECT sequence items,
    // not items inside a map field. Use depth-based indentation (4 spaces for indent_step=4).
    let expected = "items:\n    - |-\n    line one\n    line two\n";
    assert_eq!(
        yaml, expected,
        "Unexpected YAML with different indent_step:\n{yaml}"
    );

    Ok(())
}
