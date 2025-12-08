mod error;
mod config;
mod db;
mod handlers;
mod services;

use std::time::Duration;
use axum::{Router, routing::get};
use tracing_subscriber::{EnvFilter, FmtSubscriber};
use crate::{config::Config, db::{create_pool, init_db}, handlers::AppState, error::AppError};

#[tokio::main]
async fn main() -> Result<(), AppError> {
    // Инициализация логгера
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .map_err(|e| AppError::Internal(e.to_string()))?;

    // Загрузка конфигурации
    let config = Config::from_env()?;
    tracing::info!("Конфигурация загружена");

    // Подключение к БД
    let pool = create_pool(&config.database_url).await?;
    tracing::info!("Подключение к БД установлено");

    // Инициализация БД
    init_db(&pool).await?;
    tracing::info!("База данных инициализирована");

    // Создание сервисов
    let iss_service = services::IssService::new(pool.clone());
    let osdr_service = services::OsdrService::new(
        pool.clone(),
        config.nasa_api_url.clone(),
        config.nasa_api_key.clone(),
    );

    // AppState для DI
    let state = AppState {
        iss_service,
        osdr_service,
    };

    // Запуск фоновых задач
    start_background_tasks(&config, &pool);

    // Настройка маршрутов
    let app = Router::new()
        // Health check
        .route("/health", get(handlers::health))
        
        // ISS
        .route("/last", get(handlers::last_iss))
        .route("/fetch", get(handlers::trigger_iss))
        .route("/iss/trend", get(handlers::iss_trend))
        
        // OSDR
        .route("/osdr/sync", get(handlers::osdr_sync))
        .route("/osdr/list", get(handlers::osdr_list))
        
        // Space cache (упрощённые)
        .route("/space/:src/latest", get(handlers::space_latest))
        .route("/space/refresh", get(handlers::space_refresh))
        .with_state(state);

    // Запуск сервера
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await
        .map_err(|e| AppError::Internal(e.to_string()))?;
    
    tracing::info!("🚀 Сервер запущен на http://0.0.0.0:3000");
    
    axum::serve(listener, app).await
        .map_err(|e| AppError::Internal(e.to_string()))?;

    Ok(())
}


fn start_background_tasks(config: &Config, pool: &sqlx::PgPool) {
    let config_clone = config.clone();
    let pool_clone = pool.clone();
    
    tokio::spawn(async move {
        loop {
            let url = config_clone.where_iss_url.clone();
            let pool = pool_clone.clone();
            
            if let Err(e) = fetch_iss_background(&pool, &url).await {
                tracing::error!("Фоновая задача ISS: {}", e);
            }
            
            tokio::time::sleep(Duration::from_secs(config_clone.fetch_intervals.iss)).await;
        }
    });
}

async fn fetch_iss_background(pool: &sqlx::PgPool, url: &str) -> Result<(), AppError> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(20))
        .build()?;

    let response = client.get(url).send().await?;
    let status = response.status();

    if !status.is_success() {
        tracing::warn!("ISS fetch failed: {}", status);
        return Ok(());
    }

    let json: serde_json::Value = response.json().await?;

    sqlx::query(
        "INSERT INTO iss_fetch_log (source_url, payload) VALUES ($1, $2)"
    )
    .bind(url)
    .bind(json)
    .execute(pool)
    .await?;

    Ok(())
}
