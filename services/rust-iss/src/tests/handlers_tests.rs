use axum::{Router, routing::get};
use rust_iss::{handlers, services::{IssService, OsdrService}};
use serde_json::Value;
mod common;

#[tokio::test]
async fn test_health() {
    let response = handlers::health().await;
    assert_eq!(response["status"], "ok");
}

#[tokio::test]
async fn test_last_iss_handler() {
    let pool = common::setup_db().await;
    let state = rust_iss::handlers::AppState {
        iss_service: IssService::new(pool.clone()),
        osdr_service: OsdrService::new(pool.clone(), "".into(), "".into()),
    };

    sqlx::query("INSERT INTO iss_fetch_log(source_url, payload) VALUES('u', '{}')")
        .execute(&pool)
        .await
        .unwrap();

    let res = handlers::last_iss(axum::extract::State(state)).await.unwrap();
    assert!(res["data"]["id"].is_number());
}
