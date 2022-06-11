use crate::statistics::map::StatisticMapping;
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct ProgramStatistics {
    pub mapping: StatisticMapping,
}
