use std::collections::HashMap;

use crate::graph::{Name, SocketType, SocketValue};

#[derive(Debug)]
pub enum Error {
    Missing(Side, Name),
    MismatchedTypes((Name, SocketType), (Name, SocketType)),
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

/// `[get](std::collections::HashMap::get)`s the desired input/output field with error reporting
macro_rules! get_sv {
    (input | $hashmap:ident > $field:literal > $name:ident) => {
        let $name = $hashmap
            .get(&$field.into())
            .ok_or_else(|| Error::Missing(Side::Input, $field.into()))?;
    };

    (output | $hashmap:ident > $field:literal > $name:ident) => {
        let $name = $hashmap
            .get_mut(&$field.into())
            .ok_or_else(|| Error::Missing(Side::Output, $field.into()))?;
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shader_function_type() {
        let mut inputs = HashMap::new();
        inputs.insert("value".into(), SocketValue::Number(None));

        Shader::new(|inputs, outputs| {
            get_sv!(input | inputs > "value" > in_value);
            get_sv!(output | outputs > "value" > out_value);

            if !out_value.matches(in_value) {
                return Err(Error::MismatchedTypes(
                    ("value".into(), SocketType::from(in_value)),
                    ("value".into(), SocketType::from(out_value.as_ref())),
                ));
            }

            let initial = out_value.clone();

            match out_value {
                SocketValue::Number(ref mut number) => *number.get_or_insert(0.) += 1.,
                e => unimplemented!("{e:?}"),
            }

            let modified = out_value.clone();

            assert_ne!(initial, modified);

            Ok(())
        })
        .call(&inputs, &mut inputs.clone())
        .unwrap();
    }
}
