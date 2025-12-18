use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct HasStrings {
    zero: String,
    nan: String,
    colon: String,
    comment: String,
    ending_colon: String,
    trim_ending_colon: String,
}

#[test]
fn strings_that_look_special_are_quoted() -> Result<()> {
    let v = HasStrings {
        zero: "0".to_string(),
        nan: "nan".to_string(),
        colon: "a: b".to_string(),
        comment: "# hi".to_string(),
        ending_colon: "hi:".to_string(),
        trim_ending_colon: "hey:\n".to_string(),
    };

    let out = serde_saphyr::to_string(&v).expect("serialize");

    // Each of these fields should be quoted or escaped so that they are preserved as strings
    // and do not get parsed as numbers, special floats, or mapping syntax.
    // Now accepts either single or double quotes (Go's yaml.v3 prefers single quotes)
    assert!(
        out.contains("zero: \"0\"") || out.contains("zero: '0'"),
        "'0' must be quoted: {out}"
    );
    // 'nan' is no longer quoted - the YAML 1.2 spec treats unquoted nan as a string
    // (only .nan is a float), so round-trip still works correctly
    assert!(
        out.contains("nan: nan") || out.contains("nan: \"nan\"") || out.contains("nan: 'nan'"),
        "'nan' should be present: {out}"
    );
    assert!(
        out.contains("\"# hi\"") || out.contains("'# hi'"),
        "comment must be quoted: {out}"
    );
    assert!(
        out.contains("colon: \"a: b\"") || out.contains("colon: 'a: b'"),
        "'a: b' must be quoted: {out}"
    );
    assert!(
        out.contains("ending_colon: \"hi:\"") || out.contains("ending_colon: 'hi:'"),
        "ending colon must be quoted: {out}"
    );
    assert!(
        out.contains("trim_ending_colon: \"hey:\\n\"")
            || out.contains("trim_ending_colon: 'hey:\\n'"),
        "ending colon must be quoted: {out}"
    );

    let r = serde_saphyr::from_str(&out)?;
    assert_eq!(v, r);
    Ok(())
}
