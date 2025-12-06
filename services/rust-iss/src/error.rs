use axum::{http::StatusCode, Json};
use serde_json::json;

#[derive(thiserror::Error, Debug)]
pub enum AppError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    
    #[error("External API error: {0}")]
    ExternalApi(String),
    
    #[error("Configuration error: {0}")]
    Config(String),
    
    #[error("Not found")]
    NotFound,
    
    #[error("Internal error: {0}")]
    Internal(String),
}

impl AppError {
    pub fn status_code(&self) -> StatusCode {
        match self {
            AppError::Database(_) => StatusCode::INTERNAL_SERVER_ERROR,
            AppError::ExternalApi(_) => StatusCode::BAD_GATEWAY,
            AppError::Config(_) => StatusCode::INTERNAL_SERVER_ERROR,
            AppError::NotFound => StatusCode::NOT_FOUND,
            AppError::Internal(_) => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
    
    pub fn error_code(&self) -> &'static str {
        match self {
            AppError::Database(_) => "DB_ERROR",
            AppError::ExternalApi(_) => "UPSTREAM_ERROR",
            AppError::Config(_) => "CONFIG_ERROR",
            AppError::NotFound => "NOT_FOUND",
            AppError::Internal(_) => "INTERNAL_ERROR",
        }
    }
}

impl IntoResponse for AppError {
    fn into_response(self) -> axum::response::Response {
        let status = self.status_code();
        let error_response = json!({
            "ok": false,
            "error": {
                "code": self.error_code(),
                "message": self.to_string(),
                "trace_id": "req-" 
            }
        });
        
        (status, Json(error_response)).into_response()
    }
}