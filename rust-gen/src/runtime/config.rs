#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

static RUST_VERSIONS: [&str; 10] = [
    "1.26.0", "1.31.0", "1.36.0", "1.41.0", "1.46.0", "1.51.0", "1.56.0", "stable", "beta",
    "nightly",
];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
