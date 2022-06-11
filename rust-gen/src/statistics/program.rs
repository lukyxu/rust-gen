use serde::{Deserialize, Serialize};
use crate::statistics::map::StatisticMapping;

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct ProgramStatistics {
    pub mapping: StatisticMapping
}
