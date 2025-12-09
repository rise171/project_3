use axum::{
    http::{Request, HeaderName, HeaderValue},
    middleware::Next,
    response::Response,
};
use std::time::Instant;
use tracing::{info_span, Span};
use uuid::Uuid;

static TRACE_ID: HeaderName = HeaderName::from_static("x-trace-id");

pub async fn trace_layer<B>(mut req: Request<B>, next: Next<B>) -> Response {
    let trace_id = Uuid::new_v4().to_string();
    req.headers_mut()
        .insert(TRACE_ID, HeaderValue::from_str(&trace_id).unwrap());

    let span = info_span!("request", %trace_id);
    let _enter = span.enter();

    let started = Instant::now();
    let res = next.run(req).await;
    let elapsed = started.elapsed().as_millis();

    tracing::info!("{}ms", elapsed);

    let mut res = res;
    res.headers_mut()
        .insert(TRACE_ID, HeaderValue::from_str(&trace_id).unwrap());

    res
}