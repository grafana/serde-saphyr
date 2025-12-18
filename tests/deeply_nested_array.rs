use serde_saphyr::SerializerOptions;

#[test]
fn test_deeply_nested_array_line_width() {
    // EXACT options from rtk/export.rs
    let options = SerializerOptions {
        indent_step: 2,
        indent_array: Some(0),
        prefer_block_scalars: true,
        empty_map_as_braces: true,
        empty_array_as_brackets: true,
        line_width: Some(80),
        scientific_notation_threshold: None,
        ..Default::default()
    };

    // EXACT structure from the failing golden test
    let data = serde_json::json!({
        "apiVersion": "xyzw/v1",
        "kind": "Xyzzlement",
        "metadata": {
            "name": "abcd-xyzzlement",
            "namespace": "qwertyu"
        },
        "spec": {
            "template": {
                "spec": {
                    "containers": [{
                        "args": [
                            "-foobar_quxbaz_wxy_zconfig=[{\"address\": \"http://wxy-zplmnkjh.foobar-quxbaz.stu.vwxyza.bcdef.\",\"slug\": \"ghij-kl-mnop-0\",\"token\": \"$(QX_VWXYZA_BCDEFG)\"}]",
                            "xyzctl config set-vwxyza abcd-vwxyza --fghij-klmno=true --pqrstuv-wxyzabcde=/fgh/ijk/lmnop/qrstuvwx/yz.abc.def --ghijkl=https://mnopqrst.abcd.stu.vwxyza.bcdef.:443 --yzabcdefgh=/fgh/ijk/lmnopq/qrstuvwx/yzabcdefgh",
                            "-fghijkl.mnopqrst={__uvwx__=\"yzabcd_efgh\"},{__uvwx__=\"ijklmn_efgh\"}"
                        ],
                        "env": [],
                        "name": "abcd-efghijklm",
                        "volumeMounts": []
                    }],
                    "initContainers": [{
                        "args": [
                            "abcde -R fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx;yz /fgh/ijk/lmnop/qrstuvwx/yz.abc /fgh/ijk/lmnopq/qrstuvwx/yz.abc;abcde fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx/yz.abc;abcde 600 /fgh/ijk/lmnopq/qrstuvwx/yz.abc;yz /fgh/ijk/lmnop/qrstuvwx/bcdefg.hijk.abc /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.abc;abcde fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.abc;abcde 600 /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.abc;yz /fgh/ijk/lmnop/qrstuvwx/bcdefg.hijk.lmn /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.lmn;abcde fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.lmn;abcde 600 /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.lmn"
                        ],
                        "command": ["/bin/qr", "-stu"],
                        "image": "vwxybox:1.34",
                        "name": "lmnopq-rstuvw"
                    }],
                    "volumes": []
                }
            }
        }
    });

    let mut output = String::new();
    serde_saphyr::to_fmt_writer_with_options(&mut output, &data, options).unwrap();

    // Expected output - POST-check line wrapping behavior:
    // Write each word, then check if we exceeded wrap_col (80).
    // If so, wrap BEFORE the NEXT word (not the current one).
    // This matches Go yaml.v3 behavior where the first line can exceed wrap_col.
    let expected = r#"apiVersion: xyzw/v1
kind: Xyzzlement
metadata:
  name: abcd-xyzzlement
  namespace: qwertyu
spec:
  template:
    spec:
      containers:
      - args:
        - '-foobar_quxbaz_wxy_zconfig=[{"address": "http://wxy-zplmnkjh.foobar-quxbaz.stu.vwxyza.bcdef.","slug":
          "ghij-kl-mnop-0","token": "$(QX_VWXYZA_BCDEFG)"}]'
        - xyzctl config set-vwxyza abcd-vwxyza --fghij-klmno=true --pqrstuv-wxyzabcde=/fgh/ijk/lmnop/qrstuvwx/yz.abc.def
          --ghijkl=https://mnopqrst.abcd.stu.vwxyza.bcdef.:443 --yzabcdefgh=/fgh/ijk/lmnopq/qrstuvwx/yzabcdefgh
        - -fghijkl.mnopqrst={__uvwx__="yzabcd_efgh"},{__uvwx__="ijklmn_efgh"}
        env: []
        name: abcd-efghijklm
        volumeMounts: []
      initContainers:
      - args:
        - abcde -R fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx;yz /fgh/ijk/lmnop/qrstuvwx/yz.abc
          /fgh/ijk/lmnopq/qrstuvwx/yz.abc;abcde fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx/yz.abc;abcde
          600 /fgh/ijk/lmnopq/qrstuvwx/yz.abc;yz /fgh/ijk/lmnop/qrstuvwx/bcdefg.hijk.abc
          /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.abc;abcde fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.abc;abcde
          600 /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.abc;yz /fgh/ijk/lmnop/qrstuvwx/bcdefg.hijk.lmn
          /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.lmn;abcde fghijk:fghijk /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.lmn;abcde
          600 /fgh/ijk/lmnopq/qrstuvwx/bcdefg.hijk.lmn
        command:
        - /bin/qr
        - -stu
        image: vwxybox:1.34
        name: lmnopq-rstuvw
      volumes: []
"#;

    assert_eq!(
        output, expected,
        "\n\nDIFF:\nExpected:\n{}\n\nActual:\n{}",
        expected, output
    );
}
