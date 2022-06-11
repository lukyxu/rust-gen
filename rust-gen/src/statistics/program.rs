use crate::statistics::map::StatisticsMap;
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct ProgramStatistics {
    pub mapping: StatisticsMap,
}
