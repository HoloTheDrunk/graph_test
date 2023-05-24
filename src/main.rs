mod graph;
mod shader;

use std::collections::HashMap;

use graph::*;
use shader::*;

fn main() {
    let mut imported = HashMap::new();
    imported.insert(
        "identity".to_owned(),
        ImportedNode::from((
            "identity",
            graph! {
                inputs:
                    "value": SocketValue::Number(Some(0.)),
                nodes:
                    "id": node! {
                        inputs:
                            "value": (None, SocketType::Number),
                        outputs:
                            "value": SocketType::Number.into();
                        |inputs, outputs| {
                            get_sv!(input | inputs . "value" : Number > in_value);
                            get_sv!(output | outputs . "value" : Number > out_value);

                            *out_value.get_or_insert(0.) = in_value.unwrap_or(0.);

                            Ok(())
                        }
                    },
                outputs:
                    "value": (ssref!(node "id" "value"), SocketType::Number.into()),
            },
        )),
    );

    let _graph = graph! {
        inputs:
            "value": SocketValue::Number(Some(2.)),
        nodes:
            "inner": node!{
                import "identity" imported,
                inputs:
                    "value": (ssref!(graph "value"), SocketType::Number),
            },
        outputs:
            "value": (ssref!(node "inner" "value"), SocketType::Number.into()),
    }
    .validate()
    .unwrap();
}
