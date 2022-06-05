use ron::ser::PrettyConfig;
use serde::Serialize;
use std::io;

pub fn write_as_ron<W: io::Write, S: Serialize>(writer: W, object: S) {
    let mut serializer =
        ron::Serializer::new(writer, Some(PrettyConfig::default().struct_names(true)))
            .expect("Unable to create serializer");
    object
        .serialize(&mut serializer)
        .expect("Unable to serialize");
}

pub fn to_ron_string<S: Serialize>(object: S) -> String {
    ron::ser::to_string_pretty(&object, ron::ser::PrettyConfig::new().struct_names(true))
        .expect("Unable to serialize")
}
