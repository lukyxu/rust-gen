#[derive(Debug, Clone)]
pub struct OptLevel(char);
impl OptLevel {
    pub fn no_opt() -> OptLevel {
        OptLevel('0')
    }

    pub fn all_opt_levels() -> Vec<OptLevel> {
        vec![
            OptLevel('0'),
            OptLevel('1'),
            OptLevel('2'),
            OptLevel('3'),
            // OptLevel('s'),
            // OptLevel('z'),
        ]
    }
}

impl ToString for OptLevel {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

static RUST_VERSIONS: [&str; 7] = [
    "1.26.0", "1.32.0", "1.37.0", "1.42.0", "1.48.0", "1.55.0", "stable",
];

#[derive(Debug, Clone)]
pub struct RustVersion(String);
impl RustVersion {
    pub fn stable() -> RustVersion {
        RustVersion("stable".to_string())
    }

    pub fn supported_rust_versions() -> Vec<RustVersion> {
        RUST_VERSIONS.map(&str::to_string).map(RustVersion).to_vec()
    }
}

impl ToString for RustVersion {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}
