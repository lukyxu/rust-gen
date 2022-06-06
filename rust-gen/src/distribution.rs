use rand::distributions;
use rand::Rng;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Distribution {
    Uniform(usize, usize),
}

impl Distribution {
    pub fn new_uniform_inclusive(low: usize, high: usize) -> Distribution {
        assert!(low <= high);
        Distribution::Uniform(low, high)
    }

    pub fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> usize {
        match self {
            Distribution::Uniform(low, high) => {
                rng.sample(distributions::Uniform::new_inclusive(*low, *high))
            }
        }
    }

    pub fn none() -> Distribution {
        Distribution::Uniform(0, 0)
    }
}
