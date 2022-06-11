use std::cmp::min;
use rand::distributions;
use rand::Rng;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Distribution {
    Uniform(usize, usize),
    Standard(f64, f64),
}

impl Distribution {
    pub fn new_uniform_inclusive(low: usize, high: usize) -> Distribution {
        assert!(low <= high);
        Distribution::Uniform(low, high)
    }

    pub fn new_standard(mean: f64, sd: f64) -> Distribution {
        Distribution::Standard(mean, sd)
    }

    pub fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> usize {
        match self {
            Distribution::Uniform(low, high) => {
                rng.sample(distributions::Uniform::new_inclusive(*low, *high))
            }
            Distribution::Standard(mean, sd) => {
                let f64: f64 = rng.sample(distributions::Standard);
                (f64 * (*sd) + (*mean)).min(0.0) as usize
            }
        }
    }

    pub fn none() -> Distribution {
        Distribution::Uniform(0, 0)
    }
}
