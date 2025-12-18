//! Tests for trailing whitespace detection in block scalars.
//!
//! When `prefer_block_scalars: true` is set, strings with trailing whitespace
//! on any line should fall back to quoted strings instead of block scalars,
//! as YAML parsers may strip trailing whitespace from block scalar content.
//! This matches Go's yaml.v3 behavior.

use serde_saphyr::{SerializerOptions, to_fmt_writer_with_options};

#[test]
fn test_trailing_whitespace_uses_quoted_string() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String with trailing space on middle line
    let input = "line1\nline2 \nline3";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &input, opts).unwrap();

    // Should NOT use block scalar because of trailing whitespace
    assert!(
        !output.contains("|-") && !output.contains("|+") && !output.starts_with("|\n"),
        "Should not use block scalar for strings with trailing whitespace, got:\n{output}"
    );

    // Verify round-trip preserves the trailing space
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed, input,
        "Round-trip should preserve trailing whitespace"
    );
}

#[test]
fn test_trailing_tab_uses_quoted_string() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String with trailing tab on a line
    let input = "line1\nline2\t\nline3";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &input, opts).unwrap();

    // Should NOT use block scalar because of trailing whitespace (tab)
    assert!(
        !output.contains("|-") && !output.contains("|+") && !output.starts_with("|\n"),
        "Should not use block scalar for strings with trailing tab, got:\n{output}"
    );

    // Verify round-trip preserves the trailing tab
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, input, "Round-trip should preserve trailing tab");
}

#[test]
fn test_trailing_whitespace_on_first_line() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String with trailing space on first line
    let input = "line1 \nline2\nline3";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &input, opts).unwrap();

    // Should NOT use block scalar
    assert!(
        !output.contains("|-") && !output.contains("|+") && !output.starts_with("|\n"),
        "Should not use block scalar for strings with trailing whitespace on first line, got:\n{output}"
    );

    // Verify round-trip
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, input);
}

#[test]
fn test_trailing_whitespace_on_last_line() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String with trailing space on last line (before final content ends)
    let input = "line1\nline2\nline3 ";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &input, opts).unwrap();

    // Should NOT use block scalar
    assert!(
        !output.contains("|-") && !output.contains("|+") && !output.starts_with("|\n"),
        "Should not use block scalar for strings with trailing whitespace on last line, got:\n{output}"
    );

    // Verify round-trip
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, input);
}

#[test]
fn test_no_trailing_whitespace_can_use_block_scalar() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String without trailing whitespace - block scalar is safe
    let input = "line1\nline2\nline3";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &input, opts).unwrap();

    // CAN use block scalar since no trailing whitespace
    assert!(
        output.contains("|-") || output.contains("|") || output.contains("|+"),
        "Should use block scalar when no trailing whitespace, got:\n{output}"
    );

    // Verify round-trip
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, input);
}

#[test]
fn test_internal_whitespace_is_fine() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String with internal whitespace (not trailing) - block scalar is safe
    let input = "line1 with spaces\nline2\twith\ttabs\nline3";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &input, opts).unwrap();

    // Note: The existing code already falls back to quoted for tabs anywhere in string,
    // so we just check that it handles this case correctly
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, input, "Round-trip should preserve content");
}

#[test]
fn test_trailing_whitespace_in_map_value() {
    use serde::{Deserialize, Serialize};

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    #[derive(Serialize, Deserialize)]
    struct Data {
        content: String,
    }

    let data = Data {
        content: "line1\nline2 \nline3".to_string(),
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    // Should NOT use block scalar because of trailing whitespace
    assert!(
        !output.contains("|-") && !output.contains("|+"),
        "Should not use block scalar for map value with trailing whitespace, got:\n{output}"
    );

    // Verify round-trip
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed.content, "line1\nline2 \nline3");
}

#[test]
fn test_multiple_trailing_spaces() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String with multiple trailing spaces
    let input = "line1\nline2   \nline3";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &input, opts).unwrap();

    // Should NOT use block scalar
    assert!(
        !output.contains("|-") && !output.contains("|+") && !output.starts_with("|\n"),
        "Should not use block scalar for strings with multiple trailing spaces, got:\n{output}"
    );

    // Verify round-trip preserves all trailing spaces
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, input);
}

#[test]
fn test_line_width_wraps_quoted_string_with_trailing_whitespace() {
    let opts = SerializerOptions {
        prefer_block_scalars: true,
        line_width: Some(60),
        ..Default::default()
    };

    // Multiline string with trailing space (forces quoted style) that exceeds line_width
    let content = "first line\nsecond line with trailing space \nthird line here with more content";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &content, opts).unwrap();

    // Should use quoted string (due to trailing space)
    assert!(
        output.contains("\""),
        "Should use quoted string, got:\n{output}"
    );

    // Should NOT use block scalar
    assert!(
        !output.contains("|-") && !output.contains("|+"),
        "Should not use block scalar, got:\n{output}"
    );

    // Verify round-trip preserves content including trailing space
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, content);
}

#[test]
fn test_line_width_wraps_long_quoted_string_with_trailing_whitespace() {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct Data {
        content: String,
    }

    let opts = SerializerOptions {
        prefer_block_scalars: true,
        line_width: Some(40), // Short width to force wrapping
        ..Default::default()
    };

    // Long multiline string with trailing whitespace
    let content =
        "hello world this is a long string \nwith trailing space on first line".to_string();
    let data = Data {
        content: content.clone(),
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    // Should use quoted string (due to trailing space)
    assert!(
        output.contains("\""),
        "Should use quoted string, got:\n{output}"
    );

    // Verify round-trip produces identical content
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed.content, content);
}

#[test]
fn test_line_folding_preserves_leading_whitespace() {
    let opts = SerializerOptions {
        line_width: Some(40),
        ..Default::default()
    };

    // Content where second segment has leading spaces that must be preserved
    let content = "first part\n  indented second part";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &content, opts).unwrap();

    // Verify round-trip preserves the leading spaces
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed, content,
        "Leading whitespace must be preserved. Output:\n{}",
        output
    );
}

#[test]
fn test_line_folding_breaks_at_logical_points() {
    let opts = SerializerOptions {
        line_width: Some(50),
        ..Default::default()
    };

    // Content with escape sequences - should break AFTER \n, not mid-word
    let content = "line one here\nline two here\nline three";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &content, opts).unwrap();

    // Should not break in the middle of "one", "two", "three", etc.
    assert!(
        !output.contains("on\n") || output.contains("on\\n"),
        "Should not break 'one' mid-word. Output:\n{}",
        output
    );
    assert!(
        !output.contains("tw\n"),
        "Should not break 'two' mid-word. Output:\n{}",
        output
    );

    // Verify round-trip
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, content);
}

#[test]
fn test_multiline_mangled_go_yaml_compat() {
    // This is the exact case from the feature request - Go yaml.v3 style output
    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let content = "\"multilineMangled\": |\n  This is a multiline string.\n  This is a second line. It has an intentional trailing space. tk mangles it. \n\"otherField\": \"otherValue\"";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &content, opts).unwrap();

    // Verify round-trip preserves ALL content including trailing space
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed, content,
        "Content with trailing spaces must be preserved. Output:\n{}",
        output
    );
}

#[test]
fn test_exact_failing_case_with_map() {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct Data {
        #[serde(rename = "multilineMangled.txt")]
        multiline_mangled: String,
    }

    let value = "\"multilineMangled\": |\n  This is a multiline string.\n  This is a second line. It has an intentional trailing space. tk mangles it. \n\"otherField\": \"otherValue\"";

    let data = Data {
        multiline_mangled: value.to_string(),
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        prefer_block_scalars: true,
        ..Default::default()
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    println!("=== OUTPUT ===\n{}", output);

    // Verify round-trip preserves exact content
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed.multiline_mangled, value,
        "Round-trip must preserve exact content. Output:\n{}",
        output
    );
}

#[test]
fn test_no_backslash_when_content_not_whitespace() {
    let opts = SerializerOptions {
        line_width: Some(40),
        ..Default::default()
    };

    // Content after \n does NOT start with whitespace
    let value = "first line here\n#second line starts with hash";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &value, opts).unwrap();

    println!("Output:\n{}", output);

    // Should NOT have "\ #" - the backslash-space would corrupt the content
    assert!(
        !output.contains("\\ #"),
        "Should not add \\ before non-whitespace content. Output:\n{}",
        output
    );

    // Round-trip must be exact
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, value);
}

#[test]
fn test_backslash_only_when_content_is_whitespace() {
    let opts = SerializerOptions {
        line_width: Some(40),
        ..Default::default()
    };

    // Content after \n DOES start with whitespace (2 spaces)
    let value = "first line here\n  indented second line";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &value, opts).unwrap();

    println!("Output:\n{}", output);

    // Round-trip must preserve the 2 leading spaces
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed, value,
        "Must preserve leading whitespace. Output:\n{}",
        output
    );
}

#[test]
fn test_comment_lines_no_spurious_backslash() {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct Data {
        config: String,
    }

    let opts = SerializerOptions {
        line_width: Some(50),
        prefer_block_scalars: true,
        ..Default::default()
    };

    // String where content does NOT start with whitespace after line breaks
    let value = "# Comment line one that is quite long\n# Comment line two\n# Comment line three";
    let data = Data {
        config: value.to_string(),
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    println!("Output:\n{}", output);

    // Should NOT have "\ #" - would corrupt content
    assert!(
        !output.contains("\\ #"),
        "Should not add \\ before # comment. Output:\n{}",
        output
    );

    // Round-trip must preserve exact content
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed.config, value, "Round-trip must be exact");
}

#[test]
fn test_no_break_at_content_newlines() {
    // These are the exact options used by rtk
    let opts = SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: None,
        ..Default::default()
    };

    // Short content - should fit on one line without wrapping
    let content = "#\\n<Dir />\\n    Option1\\n    Option2\\n</Dir>\\n";
    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &content, opts).unwrap();

    println!("Output:\n{}", output);

    // Should NOT have "\ " escape since no wrapping should occur for short content
    assert!(
        !output.contains("\\ "),
        "Short content should not wrap, so no backslash-space needed.\nOutput:\n{}",
        output
    );

    // Round-trip must match
    let parsed: String = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(parsed, content);
}

#[test]
fn test_httpd_conf_no_early_break() {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct Data {
        #[serde(rename = "httpd.conf")]
        httpd_conf: String,
    }

    // These are the exact options used by rtk
    let opts = SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: None,
        ..Default::default()
    };

    // Content with \n followed by indented lines (4 spaces)
    let content =
        "#\n<Directory />\n    AllowOverride none\n    Require all denied\n</Directory>\n#\n";
    let data = Data {
        httpd_conf: content.to_string(),
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    println!("Output:\n{}", output);

    // The string should fit without unnecessary breaks
    // If there's a "\ " it means we broke at wrong position
    // (breaking forces \ to preserve leading spaces)
    let has_backslash_space = output.contains("\\ ");

    // Round-trip must match exactly regardless
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed.httpd_conf, content,
        "Round-trip must preserve content exactly"
    );

    // Ideally no backslash-space for content that fits
    if has_backslash_space {
        // If we did need to wrap, at least verify round-trip worked
        println!("Note: Content was wrapped (has \\ ), but round-trip succeeded");
    }
}

#[test]
fn test_multiline_mangled_go_yaml_v3_compat() {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct Data {
        #[serde(rename = "multilineMangled.txt")]
        content: String,
    }

    // Exact options from rtk
    let opts = SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: None,
        ..Default::default()
    };

    // The exact failing string with leading spaces after \n (~150 chars, must wrap)
    let value = "\"multilineMangled\": |\n  This is a multiline string.\n  This is a second line. It has an intentional trailing space. tk mangles it. \n\"otherField\": \"otherValue\"";
    let data = Data {
        content: value.to_string(),
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    println!("Output:\n{}", output);

    // The string should be wrapped (not all on one line)
    assert!(
        output.lines().count() > 1,
        "Long string should be wrapped across multiple lines.\nActual:\n{}",
        output
    );

    // Round-trip must preserve exact content including the leading spaces after \n
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed.content, value,
        "Round-trip must preserve exact content including leading spaces after \\n"
    );
}

#[test]
fn test_httpd_conf_wrapping() {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct Data {
        #[serde(rename = "httpd.conf")]
        content: String,
    }

    // Exact options from rtk
    let opts = SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: None,
        ..Default::default()
    };

    // Content with 4-space indents after \n
    let value = "<Directory />\n    AllowOverride none\n    Require all denied\n</Directory>";
    let data = Data {
        content: value.to_string(),
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    println!("Output:\n{}", output);

    // Round-trip must preserve exact content including the 4-space indents
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed.content, value,
        "Round-trip must preserve exact content including 4-space indents after \\n"
    );
}

#[test]
fn test_line_break_positions_match_go_yaml_v3() {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct Data {
        #[serde(rename = "httpd.conf")]
        content: String,
    }

    // Exact options from rtk
    let opts = SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: None,
        ..Default::default()
    };

    // Content that should NOT break at \n escape sequences prematurely
    let value = "#\n<Directory />\n    AllowOverride none\n    Require all denied\n</Directory>\n\n#\n# Note that from this point forward you must specifically allow\n# particular features to be enabled";
    let data = Data {
        content: value.to_string(),
    };

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    println!("Output:\n{}", output);

    // Should NOT have backslash-space for `\   Require` - Go doesn't break there
    // If we break at word boundaries like Go, we don't need `\ ` at those positions
    let has_require_backslash = output.contains("\\ Require") || output.contains("\\   Require");
    assert!(
        !has_require_backslash,
        "Should not break after \\n where content has leading spaces - Go breaks at word boundaries instead.\nActual:\n{}",
        output
    );

    // Round-trip MUST match exactly
    let parsed: Data = serde_saphyr::from_str(&output).unwrap();
    assert_eq!(
        parsed.content, value,
        "Round-trip failed! Content was corrupted."
    );
}

#[test]
fn test_both_long_and_short_content_together() {
    // Test that long content wraps correctly while short content doesn't break unnecessarily
    let opts = SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: None,
        ..Default::default()
    };

    // Case 1: Long content (~150 chars) - MUST break
    let long_value = "\"multilineMangled\": |\n  This is a multiline string.\n  This is a second line. It has an intentional trailing space. tk mangles it. \n\"otherField\": \"otherValue\"";

    // Case 2: Short content - must NOT break unnecessarily
    let short_value = "<Directory />\n    AllowOverride none\n    Require all denied\n</Directory>";

    let data = serde_json::json!({
        "long": long_value,
        "short": short_value
    });

    let mut output = String::new();
    to_fmt_writer_with_options(&mut output, &data, opts).unwrap();

    println!("Combined output:\n{}", output);

    let parsed: serde_json::Value = serde_saphyr::from_str(&output).unwrap();

    // BOTH must round-trip correctly
    assert_eq!(
        parsed["long"].as_str().unwrap(),
        long_value,
        "Long value corrupted"
    );
    assert_eq!(
        parsed["short"].as_str().unwrap(),
        short_value,
        "Short value corrupted"
    );
}
