//! Sub-run configuration.

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct OptLevel(char);
impl OptLevel {
    pub fn no_opt() -> OptLevel {
        OptLevel('0')
    }

    pub fn some_opt() -> OptLevel {
        OptLevel('2')
    }

    pub fn all_opt_levels() -> Vec<OptLevel> {
        vec![
            OptLevel('0'),
            OptLevel('1'),
            OptLevel('2'),
            OptLevel('3'),
            OptLevel('s'),
            OptLevel('z'),
        ]
    }
}

impl OptLevel {
    pub fn to_char(&self) -> char {
        self.0
    }
}

impl ToString for OptLevel {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

static RUST_VERSIONS: [&str; 5] = ["1.31.0", "1.41.0", "1.51.0", "stable", "nightly"];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RustVersion(String);
impl RustVersion {
    pub fn stable() -> RustVersion {
        RustVersion("stable".to_string())
    }

    pub fn supported_rust_versions() -> Vec<RustVersion> {
        RUST_VERSIONS.map(&str::to_string).map(RustVersion).to_vec()
    }

    pub fn mrustc_version() -> RustVersion {
        RustVersion("1.29.1-b364724f".to_owned())
    }

    pub fn gccrs_version() -> RustVersion {
        RustVersion("12.0.1-20220118".to_owned())
    }
}

impl ToString for RustVersion {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RustCompiler {
    RustC,
    MrustC,
    GCCRS,
}

impl ToString for RustCompiler {
    fn to_string(&self) -> String {
        match self {
            RustCompiler::RustC => "rustc",
            RustCompiler::MrustC => "mrustc",
            RustCompiler::GCCRS => "gccrs",
        }
        .to_owned()
    }
}
