use serde::Deserialize;

#[derive(Clone)]
pub struct Config {
    pub database_url: String,
    pub nasa_api_url: String,
    pub nasa_api_key: String,
    pub where_iss_url: String,
    pub fetch_intervals: FetchIntervals,
}

#[derive(Clone)]
pub struct FetchIntervals {
    pub osdr: u64,
    pub iss: u64,
    pub apod: u64,
    pub neo: u64,
    pub donki: u64,
    pub spacex: u64,
}

impl Config {
    pub fn from_env() -> Result<Self, AppError> {
        dotenvy::dotenv().ok();
        
        Ok(Self {
            database_url: std::env::var("DATABASE_URL")
                .map_err(|_| AppError::Config("DATABASE_URL is required".into()))?,
            
            nasa_api_url: std::env::var("NASA_API_URL")
                .unwrap_or_else(|_| "https://visualization.osdr.nasa.gov/biodata/api/v2/datasets/?format=json".into()),
            
            nasa_api_key: std::env::var("NASA_API_KEY").unwrap_or_default(),
            
            where_iss_url: std::env::var("WHERE_ISS_URL")
                .unwrap_or_else(|_| "https://api.wheretheiss.at/v1/satellites/25544".into()),
            
            fetch_intervals: FetchIntervals {
                osdr: env_u64("FETCH_EVERY_SECONDS", 600),
                iss: env_u64("ISS_EVERY_SECONDS", 120),
                apod: env_u64("APOD_EVERY_SECONDS", 43200),
                neo: env_u64("NEO_EVERY_SECONDS", 7200),
                donki: env_u64("DONKI_EVERY_SECONDS", 3600),
                spacex: env_u64("SPACEX_EVERY_SECONDS", 3600),
            },
        })
    }
}

fn env_u64(key: &str, default: u64) -> u64 {
    std::env::var(key).ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(default)
}