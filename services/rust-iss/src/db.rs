use sqlx::{PgPool, postgres::PgPoolOptions};
use crate::error::AppError;

pub type DbPool = PgPool;

pub async fn create_pool(database_url: &str) -> Result<DbPool, AppError> {
    PgPoolOptions::new()
        .max_connections(10)
        .connect(database_url)
        .await
        .map_err(AppError::from)
}

pub async fn init_db(pool: &DbPool) -> Result<(), AppError> {
    sqlx::query(
        "CREATE TABLE IF NOT EXISTS iss_fetch_log(
            id BIGSERIAL PRIMARY KEY,
            fetched_at TIMESTAMPTZ NOT NULL DEFAULT now(),
            source_url TEXT NOT NULL,
            payload JSONB NOT NULL
        )"
    ).execute(pool).await?;

    sqlx::query(
        "CREATE TABLE IF NOT EXISTS osdr_items(
            id BIGSERIAL PRIMARY KEY,
            dataset_id TEXT,
            title TEXT,
            status TEXT,
            updated_at TIMESTAMPTZ,
            inserted_at TIMESTAMPTZ NOT NULL DEFAULT now(),
            raw JSONB NOT NULL
        )"
    ).execute(pool).await?;

    sqlx::query(
        "CREATE UNIQUE INDEX IF NOT EXISTS ux_osdr_dataset_id
         ON osdr_items(dataset_id)"
    ).execute(pool).await?;

    sqlx::query(
        "CREATE TABLE IF NOT EXISTS space_cache(
            id BIGSERIAL PRIMARY KEY,
            source TEXT NOT NULL,
            fetched_at TIMESTAMPTZ NOT NULL DEFAULT now(),
            payload JSONB NOT NULL
        )"
    ).execute(pool).await?;

    Ok(())
}
