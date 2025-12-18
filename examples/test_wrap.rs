// Test to reproduce the exact line wrapping difference with deeper nesting
use serde::Serialize;
use serde_saphyr::{SerializerOptions, to_fmt_writer_with_options};

fn main() {
    #[derive(Serialize)]
    struct Container {
        args: Vec<String>,
    }

    #[derive(Serialize)]
    struct Spec {
        containers: Vec<Container>,
    }

    #[derive(Serialize)]
    struct Doc {
        spec: Spec,
    }

    let input = "chown -R nobody:nobody /var/run/secrets/database;cp /var/run/certs/database/ca.crt /var/run/secrets/database/ca.crt;chown nobody:nobody /var/run/secrets/database/ca.crt;chmod 600 /var/run/secrets/database/ca.crt;cp /var/run/certs/database/client.root.crt /var/run/secrets/database/client.root.crt;chown nobody:nobody /var/run/secrets/database/client.root.crt;chmod 600 /var/run/secrets/database/client.root.crt;cp /var/run/certs/database/client.root.key /var/run/secrets/database/client.root.key;chown nobody:nobody /var/run/secrets/database/client.root.key;chmod 600 /var/run/secrets/database/client.root.key";

    let doc = Doc {
        spec: Spec {
            containers: vec![Container {
                args: vec![input.to_string()],
            }],
        },
    };

    let opts = SerializerOptions {
        line_width: Some(80),
        ..Default::default()
    };

    let mut out = String::new();
    to_fmt_writer_with_options(&mut out, &doc, opts).unwrap();

    println!("=== ACTUAL OUTPUT ===");
    println!("{}", out);
    println!("=== END ===");
    
    // Print line lengths
    println!("\n=== LINE LENGTHS ===");
    for (i, line) in out.lines().enumerate() {
        println!("Line {}: {} chars - {}", i, line.len(), line);
    }
}
