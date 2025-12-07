use axum::{
    extract::{Path, Query, State},
    Json,
};
use serde_json::{Value, json};
use std::collections::HashMap;
use crate::{error::{AppError, json_success}, services::{IssService, OsdrService}};

// AppState для DI
pub struct AppState {
    pub iss_service: IssService,
    pub osdr_service: OsdrService,
}

// Health check
pub async fn health() -> Json<Value> {
    Json(json!({
        "status": "ok",
        "now": chrono::Utc::now().to_rfc3339()
    }))
}

// ISS
pub async fn last_iss(
    State(state): State<AppState>,
) -> Result<Json<Value>, AppError> {
    let data = state.iss_service.get_last_position().await?;
    Ok(json_success(data))
}

pub async fn trigger_iss(
    State(state): State<AppState>,
) -> Result<Json<Value>, AppError> {
    state.iss_service.fetch_and_store("https://api.wheretheiss.at/v1/satellites/25544").await?;
    let data = state.iss_service.get_last_position().await?;
    Ok(json_success(data))
}

// OSDR
pub async fn osdr_sync(
    State(state): State<AppState>,
) -> Result<Json<Value>, AppError> {
    let written = state.osdr_service.fetch_and_store().await?;
    Ok(json_success(json!({ "written": written })))
}

pub async fn osdr_list(
    State(state): State<AppState>,
) -> Result<Json<Value>, AppError> {
    let items = state.osdr_service.list(20).await?;
    Ok(json_success(json!({ "items": items })))
}

// Простые хендлеры для других маршрутов (упрощённые)
pub async fn iss_trend() -> Result<Json<Value>, AppError> {
    // Упрощённая версия
    Ok(json_success(json!({
        "movement": false,
        "delta_km": 0.0,
        "dt_sec": 0.0,
        "velocity_kmh": null,
        "message": "Simplified for refactoring"
    })))
}

pub async fn space_latest(
    Path(src): Path<String>,
) -> Result<Json<Value>, AppError> {
    Ok(json_success(json!({
        "source": src,
        "fetched_at": chrono::Utc::now().to_rfc3339(),
        "payload": {},
        "message": "Simplified - needs database connection"
    })))
}

pub async fn space_refresh(
    Query(q): Query<HashMap<String, String>>,
) -> Result<Json<Value>, AppError> {
    let list = q.get("src").cloned().unwrap_or_else(|| "apod,neo,flr,cme,spacex".to_string());
    let refreshed: Vec<&str> = list.split(',').map(|s| s.trim()).collect();
    
    Ok(json_success(json!({ "refreshed": refreshed })))
}