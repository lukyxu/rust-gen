use crate::statistics::map::{FullStatisticMapping, StatisticMapping};
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct GenerationStatistics {
    pub successful_mapping: StatisticMapping,
    pub failed_mapping: StatisticMapping,
    pub max_failed_item_depth: usize,
    pub max_failed_stmt_depth: usize,
    pub max_failed_expr_depth: usize,
    pub max_failed_ty_depth: usize,
}

#[derive(Default, Debug, Clone, Deserialize, Serialize)]
pub struct FullGenerationStatistics {
    pub successful_mapping: FullStatisticMapping,
    pub failed_mapping: FullStatisticMapping,
    pub max_failed_item_depth: usize,
    pub max_failed_stmt_depth: usize,
    pub max_failed_expr_depth: usize,
    pub max_failed_ty_depth: usize,
}

impl From<GenerationStatistics> for FullGenerationStatistics {
    fn from(stats: GenerationStatistics) -> FullGenerationStatistics {
        FullGenerationStatistics {
            successful_mapping: stats.successful_mapping.into(),
            failed_mapping: stats.failed_mapping.into(),
            max_failed_item_depth: stats.max_failed_item_depth,
            max_failed_stmt_depth: stats.max_failed_stmt_depth,
            max_failed_expr_depth: stats.max_failed_expr_depth,
            max_failed_ty_depth: stats.max_failed_ty_depth,
        }
    }
}
