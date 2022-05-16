use ron::ser::PrettyConfig;
use serde::Serialize;
use std::io;

pub fn write_as_ron<W: io::Write, S: Serialize>(writer: W, object: S) {
    let mut serializer = ron::Serializer::new(writer, Some(PrettyConfig::default()), true)
        .expect("Unable to create statistics serializer");
    object
        .serialize(&mut serializer)
        .expect("Unable to serialize statistics");
}
