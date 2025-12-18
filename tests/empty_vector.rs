use serde::Serialize;
use serde_saphyr::SerializerOptions;

#[test]
fn serialize_empty_vec() -> anyhow::Result<()> {
    #[derive(Serialize)]
    struct Ea {
        value_vec: Vec<usize>,
        value_array: [f32; 0],
    }

    let ea = Ea {
        value_vec: Vec::new(),
        value_array: []
    };

    // With empty_array_as_brackets: true
    let opts = SerializerOptions {
        empty_array_as_brackets: true,
        ..Default::default()
    };
    let mut s = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut s, &ea, opts)?;
    assert_eq!("value_vec: []\nvalue_array: []\n", s);

    Ok(())
}
