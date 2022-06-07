use std::process::Command;
fn main() {
    // note: add error checking yourself.
    let output = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .unwrap();
    let git_hash = String::from_utf8(output.stdout).unwrap_or("unknown".to_owned());
    println!("cargo:rustc-env=GIT_HASH={}", git_hash);

    let output = Command::new("hostname").output();
    let hostname = output
        .map(|output| String::from_utf8(output.stdout).unwrap())
        .unwrap_or("unknown".to_owned());
    println!("cargo:rustc-env=HOSTNAME={}", hostname);
}
