mod graph;
mod shader;

use std::collections::HashMap;

use graph::*;
use shader::*;

fn main() {
    let mut imported = HashMap::new();
    imported.insert(
        "invert".to_owned(),
        ImportedNode::from((
            "invert",
            graph! {
                inputs:
                    "iFac": SocketValue::Number(Some(2.)),
                nodes:
                    "invert": node! {
                        inputs:
                            "value": sref!(graph "iFac"),
                        outputs:
                            "value": SocketType::Number.into();
                        |inputs, outputs| {
                            get_sv!(input  | inputs  . "value" : Number > in_value);
                            get_sv!(output | outputs . "value" : Number > out_value);

                            *out_value.get_or_insert(0.) = 1. - in_value.unwrap_or(0.);

                            Ok(())
                        }
                    },
                outputs:
                    "oFac": (ssref!(node "invert" "value"), SocketType::Number.into()),
            },
        )),
    );

    dbg!(graph! {
        inputs:
            "iFac": SocketValue::Number(Some(2.)),
        nodes:
            "inner": node!(import "invert" imported),
        outputs:
            "oFac": (ssref!(node "invert" "value"), SocketType::Number.into()),
    }
    .validate()
    .unwrap());
}
