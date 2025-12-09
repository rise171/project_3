use std::time::Duration;
use chrono::{DateTime, Utc};
use reqwest::Client;
use serde_json::Value;
use sqlx::PgPool;
use crate::error::AppError;

pub struct IssService {
    pool: PgPool,
}

impl IssService {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }
    
    pub async fn get_last_position(&self) -> Result<Value, AppError> {
        let row = sqlx::query(
            "SELECT id, fetched_at, source_url, payload
             FROM iss_fetch_log
             ORDER BY id DESC LIMIT 1"
        ).fetch_optional(&self.pool).await?;

        match row {
            Some(row) => {
                let id: i64 = row.get("id");
                let fetched_at: DateTime<Utc> = row.get("fetched_at");
                let source_url: String = row.get("source_url");
                let payload: Value = row.try_get("payload").unwrap_or(Value::Null);
                
                Ok(serde_json::json!({
                    "id": id,
                    "fetched_at": fetched_at,
                    "source_url": source_url,
                    "payload": payload
                }))
            }
            None => Err(AppError::NotFound),
        }
    }
    
    pub async fn fetch_and_store(&self, url: &str) -> Result<(), AppError> {
        let client = Client::builder()
            .timeout(Duration::from_secs(20))
            .build()?;
        
        let response = client.get(url).send().await?;
        let json: Value = response.json().await?;
        
        sqlx::query(
            "INSERT INTO iss_fetch_log (source_url, payload) VALUES ($1, $2)"
        )
        .bind(url)
        .bind(json)
        .execute(&self.pool)
        .await?;
        
        Ok(())
    }
}

pub struct OsdrService {
    pool: PgPool,
    nasa_url: String,
    nasa_key: String,
}

impl OsdrService {
    pub fn new(pool: PgPool, nasa_url: String, nasa_key: String) -> Self {
        Self { pool, nasa_url, nasa_key }
    }
    
    pub async fn fetch_and_store(&self) -> Result<usize, AppError> {
        let client = Client::builder()
            .timeout(Duration::from_secs(30))
            .build()?;
        
        let response = client.get(url).send().await?;
        if !response.status().is_success() {
            return Err(AppError::ExternalApi(format!("HTTP {} while fetching ISS", response.status())));
        }
        let json: Value = response.json().await?;

        
        if !response.status().is_success() {
            return Err(AppError::ExternalApi(
                format!("OSDR request failed: {}", response.status())
            ));
        }
        
        let json: Value = response.json().await?;
        
        let items = if let Some(array) = json.as_array() {
            array.clone()
        } else if let Some(array) = json.get("items").and_then(|x| x.as_array()) {
            array.clone()
        } else if let Some(array) = json.get("results").and_then(|x| x.as_array()) {
            array.clone()
        } else {
            vec![json.clone()]
        };

        let mut written = 0;
        for item in items {
            let dataset_id = item.get("dataset_id")
                .or_else(|| item.get("id"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            
            let title = item.get("title")
                .or_else(|| item.get("name"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            
            let status = item.get("status")
                .or_else(|| item.get("state"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            
            // Вставка в БД
            if let Some(ds_id) = dataset_id.clone() {
                sqlx::query(
                    "INSERT INTO osdr_items(dataset_id, title, status, raw)
                     VALUES($1,$2,$3,$4)
                     ON CONFLICT (dataset_id) DO UPDATE
                     SET title=EXCLUDED.title, status=EXCLUDED.status, raw=EXCLUDED.raw"
                )
                .bind(ds_id)
                .bind(title)
                .bind(status)
                .bind(item)
                .execute(&self.pool)
                .await?;
            } else {
                sqlx::query(
                    "INSERT INTO osdr_items(dataset_id, title, status, raw)
                     VALUES($1,$2,$3,$4)"
                )
                .bind::<Option<String>>(None)
                .bind(title)
                .bind(status)
                .bind(item)
                .execute(&self.pool)
                .await?;
            }
            
            written += 1;
        }
        
        Ok(written)
    }
    
    pub async fn list(&self, limit: i64) -> Result<Vec<Value>, AppError> {
        let rows = sqlx::query(
            "SELECT id, dataset_id, title, status, updated_at, inserted_at, raw
             FROM osdr_items
             ORDER BY inserted_at DESC
             LIMIT $1"
        )
        .bind(limit)
        .fetch_all(&self.pool)
        .await?;
        
        let items: Vec<Value> = rows.into_iter().map(|row| {
            serde_json::json!({
                "id": row.get::<i64, _>("id"),
                "dataset_id": row.get::<Option<String>, _>("dataset_id"),
                "title": row.get::<Option<String>, _>("title"),
                "status": row.get::<Option<String>, _>("status"),
                "updated_at": row.get::<Option<DateTime<Utc>>, _>("updated_at"),
                "inserted_at": row.get::<DateTime<Utc>, _>("inserted_at"),
                "raw": row.get::<Value, _>("raw"),
            })
        }).collect();
        
        Ok(items)
    }
}