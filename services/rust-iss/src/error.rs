use axum::{http::StatusCode, response::{IntoResponse, Response}, Json};
use serde_json::json;
use uuid::Uuid;

#[derive(thiserror::Error, Debug)]
pub enum AppError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("External API error: {0}")]
    ExternalApi(String),

    #[error("Not found")]
    NotFound,

    #[error("Internal error: {0}")]
    Internal(String),
}

impl From<reqwest::Error> for AppError {
    fn from(err: reqwest::Error) -> Self {
        AppError::ExternalApi(err.to_string())
    }
}

impl AppError {
    fn status_code(&self) -> StatusCode {
        match self {
            AppError::Database(_) => StatusCode::INTERNAL_SERVER_ERROR,
            AppError::ExternalApi(_) => StatusCode::BAD_GATEWAY,
            AppError::NotFound => StatusCode::NOT_FOUND,
            AppError::Internal(_) => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }

    fn error_code(&self) -> &'static str {
        match self {
            AppError::Database(_) => "DB_ERROR",
            AppError::ExternalApi(_) => "UPSTREAM_ERROR",
            AppError::NotFound => "NOT_FOUND",
            AppError::Internal(_) => "INTERNAL_ERROR",
        }
    }
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let trace_id = Uuid::new_v4().to_string();

        let body = json!({
            "ok": false,
            "error": {
                "code": self.error_code(),
                "message": self.to_string(),
                "trace_id": trace_id
            }
        });

        tracing::error!(%trace_id, "{}", self);

        (self.status_code(), Json(body)).into_response()
    }
}

pub fn json_success<T: serde::Serialize>(data: T) -> Json<serde_json::Value> {
    Json(json!({
        "ok": true,
        "data": data
    }))
}
