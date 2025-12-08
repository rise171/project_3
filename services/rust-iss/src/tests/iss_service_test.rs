use rust_iss::services::IssService;
use rust_iss::error::AppError;
use serde_json::json;
mod common;

#[tokio::test]
async fn test_iss_store_and_read() -> Result<(), AppError> {
    let pool = common::setup_db().await;
    let svc = IssService::new(pool.clone());

    // вручную вставляем
    sqlx::query("INSERT INTO iss_fetch_log(source_url, payload) VALUES($1,$2)")
        .bind("test-url")
        .bind(json!({"lat": 10.0}))
        .execute(&pool)
        .await?;

    let res = svc.get_last_position().await?;
    assert_eq!(res["source_url"], "test-url");
    assert!(res["payload"].get("lat").is_some());

    Ok(())
}
