use std::{collections::HashMap, convert::AsRef, fmt::Debug};

use crate::shader::Shader;

macro_rules! socket_value {
    { $($name:ident : $type:ty),+ $(,)? } => {
        #[derive(Clone, Debug, PartialEq)]
        pub enum SocketValue {
            $(
                $name(Option<$type>)
            ),+
        }

        impl From<SocketType> for SocketValue {
            fn from(value: SocketType) -> Self {
                match value {
                    $(
                        SocketType::$name => Self::$name(None)
                    ),+
                }
            }
        }

        impl AsRef<SocketValue> for SocketValue {
            fn as_ref(&self) -> &SocketValue {
                self
            }
        }

        impl SocketValue {
            pub fn matches(&self, other: &SocketValue) -> bool {
                SocketType::from(self) == SocketType::from(other)
            }
        }

        #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
        pub enum SocketType {
            #[default]
            $($name),+
        }

        impl<T: AsRef<SocketValue>> From<T> for SocketType {
            fn from(value: T) -> Self {
                match value.as_ref() {
                    $(
                        SocketValue::$name(_) => Self::$name
                    ),+
                }
            }
        }
    };
}

socket_value! {
    Number: f32,
    String: String,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct NodeId(String);
impl From<&str> for NodeId {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Name(String);
impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SocketRef {
    Node(NodeId, Name),
    Graph(Name),
}

#[macro_export]
/// Shorthand to reference sockets from the [Graph](Graph) or other [Node](Node)s.
/// # Example
/// ```
/// sref!(graph "socket_name");
/// sref!(node "node_name" "socket_name");
/// ```
macro_rules! sref {
    (graph $field:literal) => {
        SocketRef::Graph(Name::from($field))
    };

    (node $node:literal $field:literal) => {
        SocketRef::Node(NodeId::from($node), Name::from($field))
    };
}

#[macro_export]
macro_rules! ssref {
    ($($tree:tt)+) => {
        Some(sref!($($tree)+))
    };
}

#[derive(Debug, Default, PartialEq)]
pub struct Graph {
    pub inputs: HashMap<Name, SocketValue>,
    pub outputs: HashMap<Name, (Option<SocketRef>, SocketValue)>,

    pub nodes: HashMap<NodeId, Node>,
}

#[macro_export]
macro_rules! graph {
    { $($field:ident : $($name:literal : $value:expr),+),+ $(,)? } => {
        Graph {
            $($field: [$(($name.into(), $value)),+].into_iter().collect()),+
        }
    };
}

#[derive(Default)]
pub struct Node {
    pub inputs: HashMap<Name, SocketRef>,
    pub outputs: HashMap<Name, SocketValue>,

    pub shader: Shader,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("inputs", &self.inputs)
            .field("outputs", &self.outputs)
            .finish_non_exhaustive()
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        // Ignore shader
        self.inputs == other.inputs && self.outputs == other.outputs
    }
}

// #[derive(Default)]
// pub struct ImportedNode(Name);
//
// pub trait Node {}

#[macro_export]
/// Instantiate a node concisely
/// # Example
/// ```
/// node! {
///     inputs:
///         "value": sref!(graph "iFac"),
///     outputs:
///         "value": SocketValue::Number(None);
///     |_inputs, _outputs| ()
/// }
/// ```
/// The shader is optional and will be defaulted if empty.
///
/// See [Shader::new] for an example function.
macro_rules! node {
    { import $name:literal } => {
        ImportedNode($name)
    };

    { $($field:ident : $($o_name:literal : $value:expr),+),+; $shader:expr $(,)? } => {
        Node {
            $($field: [$(($o_name.into(), $value)),+].into_iter().collect()),+,
            shader: Shader::new($shader),
        }
    };

    { $($field:ident : $($o_name:literal : $value:expr),+),+ $(,)? $(;)? } => {
        Node {
            $($field: [$(($o_name.into(), $value)),+].into_iter().collect()),+,
            shader: Shader::default(),
        }
    };
}

// Export macros
pub use {graph, node, sref, ssref};

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn macro_validity() {
        let manual = Graph {
            inputs: [
                (Name::from("iFac"), SocketValue::Number(Some(2.))),
                (Name::from("iName"), SocketValue::String(None)),
            ]
            .into_iter()
            .collect(),
            nodes: [
                (
                    NodeId::from("identity"),
                    Node {
                        inputs: std::iter::once((
                            Name::from("value"),
                            SocketRef::Graph(Name::from("iFac")),
                        ))
                        .collect(),
                        outputs: std::iter::once((Name::from("value"), SocketValue::Number(None)))
                            .collect(),
                        ..Default::default()
                    },
                ),
                (
                    NodeId::from("invert"),
                    Node {
                        inputs: std::iter::once((
                            Name::from("value"),
                            SocketRef::Node(NodeId::from("identity"), Name::from("value")),
                        ))
                        .collect(),
                        outputs: [(Name::from("value"), SocketValue::Number(None))]
                            .into_iter()
                            .collect(),
                        ..Default::default()
                    },
                ),
            ]
            .into_iter()
            .collect(),
            outputs: std::iter::once((
                Name::from("oFac"),
                (
                    Some(SocketRef::Node(NodeId::from("invert"), Name::from("value"))),
                    SocketValue::Number(None),
                ),
            ))
            .collect(),
        };

        let r#macro = graph! {
            inputs:
                "iFac": SocketValue::Number(Some(2.)),
                "iName": SocketValue::String(None),
            nodes:
                "identity": node! {
                    inputs:
                        "value": sref!(graph "iFac"),
                    outputs:
                        "value": SocketValue::Number(None)
                },
                "invert": node! {
                    inputs:
                        "value": sref!(node "identity" "value"),
                    outputs:
                        "value": SocketValue::Number(None);
                },
            outputs:
                "oFac": (ssref!(node "invert" "value"), SocketValue::Number(None)),
        };

        assert_eq!(manual, r#macro);
    }
}
