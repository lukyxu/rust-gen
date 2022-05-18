use rust_gen::runtime::config::RustVersion;
use std::process::Command;

fn main() {
    for version in RustVersion::supported_rust_versions() {
        match install_toolchain(&version) {
            Ok(()) => println!("Successfully installed version: {}", version.to_string()),
            Err(err) => eprintln!("{}", err),
        }
    }
}

fn install_toolchain(version: &RustVersion) -> Result<(), String> {
    let output = Command::new("rustup")
        .args(["install", &version.to_string()])
        .output()
        .map_err(|_| "Failed to execute install process")?;

    if !output.status.success() {
        return Err(format!(
            "Failed to install version: {}\n{}",
            version.to_string(),
            String::from_utf8_lossy(output.stderr.as_ref())
        ));
    }
    Ok(())
}
