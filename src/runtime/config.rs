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

#[derive(Debug, Clone)]
pub struct RustVersion(String);
impl RustVersion {
    pub fn stable() -> RustVersion {
        RustVersion("stable".to_string())
    }

    pub fn supported_rust_versions() -> Vec<RustVersion> {
        vec![
            RustVersion("1.26.0".to_string()),
            RustVersion("1.42.0".to_string()),
        ]
    }
}

impl ToString for RustVersion {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}
