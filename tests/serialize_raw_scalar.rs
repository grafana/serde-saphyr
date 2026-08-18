use std::collections::BTreeMap;
use std::rc::Rc;

use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};
use serde_saphyr::{Commented, FlowMap, FlowSeq, RAW_SCALAR_TOKEN, RawScalar, RcAnchor, to_string};

#[derive(Serialize)]
struct Document<'a> {
    number: RawScalar<'a>,
    values: Vec<RawScalar<'a>>,
}

#[test]
fn raw_scalars_are_emitted_exactly_in_block_and_flow_contexts() {
    assert_eq!(to_string(&RawScalar("1e+06")).unwrap(), "1e+06\n");
    assert_eq!(
        to_string(&Document {
            number: RawScalar("9.007199254740992e+15"),
            values: vec![RawScalar("-0.0"), RawScalar("1e-100")],
        })
        .unwrap(),
        "number: 9.007199254740992e+15\nvalues:\n  - -0.0\n  - 1e-100\n"
    );
    assert_eq!(
        to_string(&FlowSeq(vec![RawScalar("1e+06"), RawScalar("1e-05")])).unwrap(),
        "[1e+06, 1e-05]\n"
    );

    let map = BTreeMap::from([("number", RawScalar("1e+100"))]);
    assert_eq!(to_string(&FlowMap(map)).unwrap(), "{number: 1e+100}\n");
}

#[test]
fn raw_scalars_integrate_with_comments_and_anchors() {
    assert_eq!(
        to_string(&Commented(RawScalar("1e+06"), "exact".into())).unwrap(),
        "1e+06 # exact\n"
    );

    let shared = Rc::new(RawScalar("1e+06"));
    assert_eq!(
        to_string(&vec![RcAnchor(shared.clone()), RcAnchor(shared)]).unwrap(),
        "- &a1 1e+06\n- *a1\n"
    );
}

#[test]
fn another_serializer_sees_a_sentinel_object_not_a_string() {
    assert_eq!(
        serde_json::to_string(&RawScalar("1e+06")).unwrap(),
        format!(r#"{{"{RAW_SCALAR_TOKEN}":"1e+06"}}"#)
    );
}

#[test]
fn unsafe_yaml_source_is_rejected() {
    for value in [
        "",
        "---",
        "...",
        " leading",
        "trailing ",
        "x\nkind: Pod",
        "x # comment",
        "a: b",
        "[1, 2]",
        "{a: b}",
        "&anchor",
        "*alias",
        "!tag value",
    ] {
        assert!(to_string(&RawScalar(value)).is_err(), "accepted {value:?}");
    }
}

struct FakeRawScalar<T> {
    field: &'static str,
    value: T,
}

impl<T: Serialize> Serialize for FakeRawScalar<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut state = serializer.serialize_struct(RAW_SCALAR_TOKEN, 1)?;
        state.serialize_field(self.field, &self.value)?;
        state.end()
    }
}

#[test]
fn fabricated_sentinels_are_validated_by_the_serializer() {
    assert!(
        to_string(&FakeRawScalar {
            field: "wrong",
            value: "1e+06"
        })
        .is_err()
    );
    assert!(
        to_string(&FakeRawScalar {
            field: RAW_SCALAR_TOKEN,
            value: 1_u64
        })
        .is_err()
    );
    assert!(
        to_string(&FakeRawScalar {
            field: RAW_SCALAR_TOKEN,
            value: "x\ny"
        })
        .is_err()
    );
}
