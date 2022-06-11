use crate::statistics::map::{FullStatisticsMap, StatisticsMap};
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct ProgramStatistics {
    pub mapping: StatisticsMap,
}

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct FullProgramStatistics {
    pub mapping: FullStatisticsMap,
}

impl From<ProgramStatistics> for FullProgramStatistics {
    fn from(stats: ProgramStatistics) -> Self {
        FullProgramStatistics {
            mapping: stats.mapping.into(),
        }
    }
}
