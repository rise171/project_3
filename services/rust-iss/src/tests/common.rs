use sqlx::{Pool, Postgres};
use std::sync::Once;

static INIT: Once = Once::new();

pub async fn setup_db() -> Pool<Postgres> {
    INIT.call_once(|| {
        dotenvy::dotenv().ok();
    });

    let database_url = std::env::var("DATABASE_URL")
        .unwrap_or("postgres://postgres:password@localhost:5432/test_db".to_string());

    let pool = sqlx::PgPool::connect(&database_url)
        .await
        .expect("DB connect failed");

    sqlx::query("DROP SCHEMA public CASCADE; CREATE SCHEMA public;")
        .execute(&pool)
        .await
        .ok();

    crate::db::init_db(&pool).await.expect("DB init failed");
    pool
}
