use std::{
    collections::{HashMap, VecDeque},
    convert::AsRef,
    fmt::Debug,
    marker::PhantomData,
};

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

        // impl SocketValue {
        //     pub fn matches(&self, other: &SocketValue) -> bool {
        //         SocketType::from(self) == SocketType::from(other)
        //     }
        // }

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
impl From<&Name> for String {
    fn from(name: &Name) -> Self {
        name.0.clone()
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
/// let graph_socket = sref!(graph "socket_name");
/// assert_eq!(graph_socket, SocketRef::Graph(Name::from("socket_name")));
///
/// let node_socket = sref!(node "node_name" "socket_name");
/// assert_eq!(node_socket, SocketRef::Node(Name::from("node_name"), Name::from("socket_name")));
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
/// Shorthand to reference sockets from the [Graph]s or other [Node]s, wrapped in an
/// [Option::Some]. Calls [sref] internally so the syntax is the same.
/// # Example
/// ```
/// assert_eq!(ssref!(graph "value"), Some(sref!(graph "value")));
/// ```
macro_rules! ssref {
    ($($tree:tt)+) => {
        Some(sref!($($tree)+))
    };
}

macro_rules! states {
    ($($state:tt),+ $(,)?) => {
        $(
            #[derive(Clone, Debug, Default, PartialEq)]
            pub struct $state;
        )+
    };
}
states!(Unvalidated, Validated);

#[derive(Debug, PartialEq)]
pub enum Error {
    Cycle(NodeId),
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Graph<State> {
    pub inputs: HashMap<Name, SocketValue>,
    pub outputs: HashMap<Name, (Option<SocketRef>, SocketValue)>,

    pub nodes: HashMap<NodeId, Node<State>>,

    pub state: PhantomData<State>,
}

#[macro_export]
macro_rules! graph {
    { $($field:ident $(: $($name:literal : $value:expr),+)? $(,)?),+ } => {
        Graph {
            $($field: [$($(($name.into(), $value)),+)?].into_iter().collect()),+,
            state: ::std::marker::PhantomData::<Unvalidated>,
        }
    };
}

impl Graph<Unvalidated> {
    pub fn validate(self) -> Result<Graph<Validated>, Error> {
        for output in self.outputs.iter() {
            let mut path = Vec::<&NodeId>::new();
            let mut next = VecDeque::new();
            next.push_back({
                let (_name, (Some(socket_ref), _value)) = output else {continue};
                match socket_ref {
                    SocketRef::Node(node_id, _name) => Some(node_id),
                    SocketRef::Graph(_name) => None,
                }
            });

            while let Some(Some(id)) = next.pop_front() {
                // Check for cycle in graph
                if path.contains(&id) {
                    return Err(Error::Cycle(id.clone()));
                }

                path.push(id);

                self.nodes.get(id).map(|node| {
                    let inputs = match node {
                        Node::Graph(graph_node) => graph_node.inputs.values(),
                        Node::Imported(imported_node) => imported_node.inputs.values(),
                    };

                    for socket_ref in inputs.flatten() {
                        match socket_ref {
                            SocketRef::Node(node_id, _name) => next.push_back(Some(node_id)),
                            // Ignore graph inputs
                            SocketRef::Graph(_name) => continue,
                        }
                    }
                });
            }
        }

        let Self {
            inputs,
            outputs,
            nodes,
            state: _state,
        } = self;

        Ok(Graph {
            inputs,
            outputs,
            nodes: nodes
                .into_iter()
                .map(|(k, v)| Ok((k, v.validate()?)))
                .collect::<Result<_, Error>>()?,
            state: PhantomData::<Validated>,
        })
    }
}

#[derive(Clone, Default)]
pub struct GraphNode {
    pub inputs: HashMap<Name, Option<SocketRef>>,
    pub outputs: HashMap<Name, SocketValue>,

    pub shader: Shader,
}

impl Debug for GraphNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("inputs", &self.inputs)
            .field("outputs", &self.outputs)
            .finish_non_exhaustive()
    }
}

impl PartialEq for GraphNode {
    fn eq(&self, other: &Self) -> bool {
        // Ignore shader
        self.inputs == other.inputs && self.outputs == other.outputs
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ImportedNode<State> {
    name: Name,
    pub inputs: HashMap<Name, Option<SocketRef>>,
    inner: Graph<State>,
}

impl<State> ImportedNode<State> {
    pub fn name(&self) -> &Name {
        &self.name
    }
}

impl<T: AsRef<str>, State> From<(T, Graph<State>)> for ImportedNode<State> {
    fn from((name, inner): (T, Graph<State>)) -> Self {
        Self {
            inputs: inner
                .inputs
                .keys()
                .map(|name| (name.clone(), None))
                .collect(),
            name: Name::from(name.as_ref()),
            inner,
        }
    }
}

impl ImportedNode<Unvalidated> {
    fn validate(self) -> Result<ImportedNode<Validated>, Error> {
        let ImportedNode {
            name,
            inputs,
            inner,
        } = self;

        Ok(ImportedNode {
            name,
            inputs,
            inner: inner.validate()?,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node<State> {
    Graph(GraphNode),
    Imported(ImportedNode<State>),
}

impl<State> Default for Node<State> {
    fn default() -> Self {
        Self::Graph(GraphNode::default())
    }
}

impl Node<Unvalidated> {
    fn validate(self) -> Result<Node<Validated>, Error> {
        Ok(match self {
            Node::Graph(node) => Node::Graph(node),
            Node::Imported(node) => Node::Imported(node.validate()?),
        })
    }
}

#[macro_export]
/// Instantiate a node concisely
/// # Example
/// ```
/// node! {
///     inputs:
///         "value": ssref!(graph "iFac"),
///     outputs:
///         "value": SocketValue::Number(None);
///     |_inputs, _outputs| ()
/// }
/// ```
/// The shader is optional and will be defaulted if empty.
///
/// See [Shader::new] for an example function.
macro_rules! node {
    ( import $name:literal $imported:expr $(,)?) => {
        Node::Imported(
            $imported
                .get($name)
                .expect(format!("Could not find imported node `{}`. Imported nodes are: {}",
                    $name, $imported.keys().cloned().collect::<Vec<String>>().join(", ")).as_str()).clone()
        )
    };

    ( import $name:literal $imported:expr $(, inputs: $($input:literal : $socket_ref:expr)+)? $(,)?) => {
        Node::Imported({
            let mut res = $imported
                .get($name)
                .expect(format!("Could not find imported node `{}`. Imported nodes are: {}",
                    $name, $imported.keys().cloned().collect::<Vec<String>>().join(", ")).as_str()).clone();

            $(
                $(
                    let len = res.inputs.len();
                    let inputs = res
                        .inputs
                        .keys()
                        .map(String::from)
                        .collect::<Vec<String>>()
                        .join(", ");

                    *res.inputs.get_mut(&$input.into()).expect(
                        format!(
                            "Could not find input `{}` for node `{}`. Node's inputs are: ({}) [{}]",
                            $input, $name, len, inputs
                        )
                        .as_str(),
                    ) = $socket_ref;
                )+
            )?

            res
        })
    };

    { $($field:ident $(: $($o_name:literal : $value:expr),+)?),+; $shader:expr $(,)? } => {
        Node::Graph(GraphNode {
            $($field: [$($(($o_name.into(), $value)),+)?].into_iter().collect()),+,
            shader: Shader::new($shader),
        })
    };

    { $($field:ident $(: $($o_name:literal : $value:expr),+)?),+ $(,)? $(;)? } => {
        Node::Graph(GraphNode {
            $($field: [$($(($o_name.into(), $value)),+)?].into_iter().collect()),+,
            shader: Shader::default(),
        })
    };
}

// Export macros
pub use {graph, node, sref, ssref};

#[cfg(test)]
mod test {
    use super::*;
    use crate::get_sv;

    fn setup_imports() -> HashMap<String, ImportedNode<Unvalidated>> {
        std::iter::once((
            "identity".to_owned(),
            ImportedNode::from((
                "identity",
                graph! {
                    inputs:
                        "value": SocketValue::Number(Some(0.)),
                    nodes:
                        "id": node! {
                            inputs:
                                "value": None,
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
        ))
        .collect()
    }

    #[test]
    fn cycle_detection() {
        let imported = setup_imports();

        let validation_result = graph! {
            inputs,
            nodes:
                "a": node! {
                    import "identity" imported,
                    inputs:
                        "value": ssref!(node "b" "value"),
                },
                "b": node! {
                    import "identity" imported,
                    inputs:
                        "value": ssref!(node "a" "value"),
                },
            outputs:
                "value": (ssref!(node "a" "value"), SocketType::Number.into()),
        }
        .validate();

        let expected = Error::Cycle(NodeId("a".to_owned()));

        assert!(
            validation_result.is_err(),
            "Expected an error, got `{validation_result:?}`"
        );

        assert_eq!(validation_result.unwrap_err(), expected);
    }

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
                    GraphNode {
                        inputs: std::iter::once((
                            Name::from("value"),
                            Some(SocketRef::Graph(Name::from("iFac"))),
                        ))
                        .collect(),
                        outputs: std::iter::once((Name::from("value"), SocketValue::Number(None)))
                            .collect(),
                        ..Default::default()
                    },
                ),
                (
                    NodeId::from("invert"),
                    GraphNode {
                        inputs: std::iter::once((
                            Name::from("value"),
                            Some(SocketRef::Node(
                                NodeId::from("identity"),
                                Name::from("value"),
                            )),
                        ))
                        .collect(),
                        outputs: [(Name::from("value"), SocketValue::Number(None))]
                            .into_iter()
                            .collect(),
                        ..Default::default()
                    },
                ),
            ]
            .map(|(name, node)| (name, Node::Graph(node)))
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
            state: PhantomData::<Unvalidated>,
        };

        let r#macro = graph! {
            inputs:
                "iFac": SocketValue::Number(Some(2.)),
                "iName": SocketValue::String(None),
            nodes:
                "identity": node! {
                    inputs:
                        "value": ssref!(graph "iFac"),
                    outputs:
                        "value": SocketType::Number.into()
                },
                "invert": node! {
                    inputs:
                        "value": ssref!(node "identity" "value"),
                    outputs:
                        "value": SocketType::Number.into();
                },
            outputs:
                "oFac": (ssref!(node "invert" "value"), SocketValue::Number(None)),
        };

        assert_eq!(manual, r#macro);
    }
}
