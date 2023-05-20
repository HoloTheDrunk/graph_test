use std::collections::HashMap;

use crate::graph::{Name, SocketType, SocketValue};

#[derive(Debug)]
pub enum Error {
    Missing(Side, Name),
    MismatchedTypes((Name, SocketType), (Name, SocketType)),
    InvalidType {
        name: Name,
        got: SocketType,
        expected: SocketType,
    },
    Unknown,
}

#[derive(Debug)]
pub enum Side {
    Input,
    Output,
}

pub struct Shader {
    func: Box<dyn CloneFn>,
}

impl Shader {
    /// Creates a [Shader] from a shader function.
    /// # Example
    /// ```
    /// Shader::new(|inputs, outputs| {
    ///     get_sv!(input  | inputs  . "value" : Number > in_value);
    ///     get_sv!(output | outputs . "value" : Number > out_value);
    ///
    ///     *out_value.get_or_insert(0.) = 1. - in_value.unwrap_or(0.);
    ///
    ///     Ok(())
    /// })
    /// ```
    pub fn new(
        func: fn(&HashMap<Name, SocketValue>, &mut HashMap<Name, SocketValue>) -> Result<(), Error>,
    ) -> Self {
        Self {
            func: Box::new(func),
        }
    }

    pub fn call(
        &self,
        inputs: &HashMap<Name, SocketValue>,
        outputs: &mut HashMap<Name, SocketValue>,
    ) -> Result<(), Error> {
        (self.func)(inputs, outputs)
    }
}

impl Default for Shader {
    fn default() -> Self {
        Self::new(|_inputs, _outputs| Ok(()))
    }
}

impl Clone for Shader {
    fn clone(&self) -> Self {
        Self {
            func: self.func.clone_box(),
        }
    }
}

pub trait CloneFn:
    Fn(&HashMap<Name, SocketValue>, &mut HashMap<Name, SocketValue>) -> Result<(), Error>
{
    fn clone_box(&self) -> Box<dyn CloneFn>;
}

impl<F: Clone + 'static> CloneFn for F
where
    F: Fn(&HashMap<Name, SocketValue>, &mut HashMap<Name, SocketValue>) -> Result<(), Error>,
{
    fn clone_box(&self) -> Box<dyn CloneFn> {
        Box::new(self.clone())
    }
}

// macro_rules! get_svs {
//     ($inputs:ident $(: $($input:literal),+)? | $outputs:ident $(: $($output:literal),+)?) => {
//         (
//             [$($($input),+)?]
//             .into_iter()
//             .map(|name: &str| $inputs
//                 .get(&name.into())
//                 .ok_or_else(|| Error::Missing(Side::Input, name.into()))
//             ).collect::<Result<Vec<&SocketValue>, Error>>()?,
//
//             [$($($output),+)?]
//             .into_iter()
//             .map(|name: &str| $outputs
//                 .get(&name.into())
//                 .map(|res| res.borrow_mut())
//                 .ok_or_else(move || Error::Missing(Side::Output, name.into()))
//             ).collect::<Result<Vec<RefMut<SocketValue>>, Error>>()?,
//         )
//     };
// }
//
// macro_rules! unpack_svs {
//     ($svs:expr => $($input:ident),+ | $($output:ident),+) => {
//         let (input_vec, output_vec): (Vec<_>, Vec<_>) = $svs;
//         let (mut input_iter, mut output_iter) = (input_vec.into_iter(), output_vec.into_iter());
//
//         $(
//             let $input = input_iter.next().unwrap();
//         )+
//
//         $(
//             let mut $output = output_iter.next().unwrap();
//         )+
//     };
// }

#[macro_export]
/// [get](std::collections::HashMap::get)s the desired input/output field with error reporting
macro_rules! get_sv {
    (input | $hashmap:ident . $field:literal : $type:ident > $name:ident) => {
        let $name = $hashmap
            .get(&$field.into())
            .ok_or_else(|| Error::Missing(Side::Input, $field.into()))?;

        #[rustfmt::skip]
        let SocketValue::$type($name) = $name
            else {
                return Err(Error::InvalidType {
                    name: $field.into(),
                    got: SocketType::from($name),
                    expected: SocketType::$type,
                });
            };
    };

    (output | $hashmap:ident . $field:literal : $type:ident > $name:ident) => {
        let $name = $hashmap
            .get_mut(&$field.into())
            .ok_or_else(|| Error::Missing(Side::Output, $field.into()))?;

        #[rustfmt::skip]
        let SocketValue::$type(ref mut $name) = $name
            else {
                return Err(Error::InvalidType {
                    name: $field.into(),
                    got: SocketType::from($name),
                    expected: SocketType::$type,
                });
            };
    };
}

pub use get_sv;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shader_function_type() {
        let mut inputs = HashMap::new();
        inputs.insert("value".into(), SocketType::Number.into());

        Shader::new(|inputs, outputs| {
            get_sv!( input | inputs  . "value" : Number > in_value);
            get_sv!(output | outputs . "value" : Number > out_value);

            let initial = out_value.clone();

            *out_value.get_or_insert(0.) += in_value.unwrap_or(0.);

            let modified = out_value.clone();

            assert_ne!(initial, modified);

            Ok(())
        })
        .call(&inputs, &mut inputs.clone())
        .unwrap();
    }
}
