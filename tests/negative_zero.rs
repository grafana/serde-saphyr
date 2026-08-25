//! Spelling a negative zero the way Go does.

fn go_style() -> serde_saphyr::SerializerOptions {
    serde_saphyr::SerializerOptions {
        scientific_notation_threshold: Some(1_000_000),
        scientific_notation_small_threshold: Some(0.0001),
        go_style_negative_zero: true,
        ..Default::default()
    }
}

fn emit(value: &serde_json::Value, options: serde_saphyr::SerializerOptions) -> String {
    let mut out = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut out, value, options).unwrap();
    out
}

/// `strconv.FormatFloat(f, 'g', -1, 64)` writes a negative zero without a
/// fractional part, and `gopkg.in/yaml.v2` and `yaml.v3` follow it.
#[test]
fn go_style_writes_negative_zero_without_a_point() {
    assert_eq!(emit(&serde_json::json!(-0.0), go_style()), "-0\n");
}

/// Off by default, so existing output does not move.
#[test]
fn the_shortest_representation_is_still_the_default() {
    let mut options = go_style();
    options.go_style_negative_zero = false;
    assert_eq!(emit(&serde_json::json!(-0.0), options), "-0.0\n");
}

/// Only a negative zero is affected.
#[test]
fn nothing_else_changes_spelling() {
    for (value, expected) in [
        (serde_json::json!(0.0), "0.0\n"),
        (serde_json::json!(-1.5), "-1.5\n"),
        (serde_json::json!(-0.5), "-0.5\n"),
        (serde_json::json!(0.0001), "0.0001\n"),
    ] {
        assert_eq!(emit(&value, go_style()), expected, "for {value}");
    }
}

/// The spelling does not depend on how scientific notation is configured: a
/// threshold of zero would otherwise route every zero through the exponent
/// path and emit `-0e+00`.
#[test]
fn the_spelling_survives_an_aggressive_scientific_threshold() {
    let mut options = go_style();
    options.scientific_notation_threshold = Some(0);
    assert_eq!(emit(&serde_json::json!(-0.0), options), "-0\n");
}

/// Nested, since a manifest is where this actually shows up.
#[test]
fn applies_inside_maps_and_sequences() {
    let value = serde_json::json!({ "zeros": [-0.0, 0.0], "zero": -0.0 });
    assert_eq!(
        emit(&value, go_style()),
        "zero: -0\nzeros:\n  - -0\n  - 0.0\n"
    );
}
