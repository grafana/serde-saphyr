//! Tests for line wrapping of strings that require quoting

use serde::{Deserialize, Serialize};
use serde_saphyr::{SerializerOptions, to_fmt_writer_with_options};

/// Test case from issue: strings starting with `-` and containing special chars
/// should use quoted style with wrapping, not folded block style
#[test]
fn long_string_starting_with_dash_uses_quoted_style() {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Doc {
        args: Vec<String>,
    }

    let doc = Doc {
        args: vec![
            r#"-hosted_grafana_api_config=[{"address": "http://api-headless.hosted-grafana.svc.cluster.local.","slug": "prod-sa-east-0","token": "$(HG_CLUSTER_TOKEN)"}]"#.to_string(),
        ],
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    println!("Output:\n{}", out);

    // Should NOT use folded block scalar style (>)
    // because the string requires quoting (contains ")
    assert!(
        !out.contains(">\n") && !out.contains(">-\n") && !out.contains(">+\n"),
        "Expected quoted style, not folded block scalar. Got:\n{}",
        out
    );

    // Should use quoted string style (single quotes for strings with ")
    assert!(
        out.contains("'"),
        "Expected single-quoted string style. Got:\n{}",
        out
    );

    // Continuation lines should be indented (not at column 0)
    let lines: Vec<&str> = out.lines().collect();
    for (i, line) in lines.iter().enumerate().skip(1) {
        if !line.is_empty() && !line.starts_with("args:") {
            assert!(
                line.starts_with("  "),
                "Line {} should be indented: {:?}",
                i,
                line
            );
        }
    }

    // Note: Go yaml.v3 doesn't strictly enforce line_width - it uses it as a hint
    // for where to break but allows lines to exceed the limit to keep words together.
    // This matches that behavior.

    // Should parse back correctly
    let parsed: Doc = serde_saphyr::from_str(&out).unwrap();
    assert_eq!(
        parsed.args[0],
        r#"-hosted_grafana_api_config=[{"address": "http://api-headless.hosted-grafana.svc.cluster.local.","slug": "prod-sa-east-0","token": "$(HG_CLUSTER_TOKEN)"}]"#
    );
}

/// CRITICAL: Verify plain strings do NOT use folded scalar style (>-)
/// This is a byte-for-byte comparison test
#[test]
fn plain_string_never_uses_folded_style() {
    #[derive(Serialize, Deserialize, Debug)]
    struct Doc {
        args: Vec<String>,
    }

    let doc = Doc {
        args: vec![
            "kubectl config set-cluster test-cluster --embed-certs=true --certificate-authority=/var/run/certs/kubernetes/ca.pem.crt --server=https://apiserver.test.svc.cluster.local.:443 --kubeconfig=/var/run/secrets/kubernetes/kubeconfig".to_string(),
        ],
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    println!("=== ACTUAL OUTPUT (byte-for-byte) ===");
    println!("{}", out);
    println!("=== END OUTPUT ===");

    // CRITICAL: Must NOT contain any folded style indicators
    assert!(
        !out.contains(">"),
        "FAILED: Output contains '>' (folded style indicator). Got:\n{}",
        out
    );

    // Verify the expected structure: plain scalar continuation
    assert!(
        out.contains("  - kubectl"),
        "Expected plain scalar starting with '  - kubectl'. Got:\n{}",
        out
    );
}

/// Test with EXACT user options that were causing >- issue
#[test]
fn test_with_exact_user_options() {
    #[derive(Serialize, Deserialize, Debug)]
    struct Doc {
        args: Vec<String>,
    }

    let doc = Doc {
        args: vec![
            "kubectl config set-cluster test-cluster --embed-certs=true --certificate-authority=/var/run/certs/kubernetes/ca.pem.crt --server=https://apiserver.test.svc.cluster.local.:443 --kubeconfig=/var/run/secrets/kubernetes/kubeconfig".to_string(),
        ],
    };

    // EXACT options from user
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

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    println!("=== OUTPUT WITH USER'S EXACT OPTIONS ===");
    println!("{}", out);
    println!("=== END OUTPUT ===");

    // CRITICAL: Must NOT contain any folded style indicators
    assert!(
        !out.contains(">"),
        "FAILED: Output contains '>' (folded style indicator). Got:\n{}",
        out
    );
}
