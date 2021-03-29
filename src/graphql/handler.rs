use crate::cli_args::Opt;
use crate::database::{db_connection, Pool};
use crate::graphql::model::{Context, Schema};
use actix_web::{error, web, Error, HttpResponse};
use juniper::http::playground::playground_source;
use juniper::http::GraphQLRequest;

pub(super) async fn graphql(
    st: web::Data<Schema>,
    data: web::Json<GraphQLRequest>,
    pool: web::Data<Pool>,
    opt: web::Data<Opt>,
) -> Result<HttpResponse, Error> {
    let db_pool = db_connection(&pool)?;

    let opt = opt.into_inner().as_ref().clone();
    let ctx = Context::new(db_pool, opt);

    let res = data.execute(&st, &ctx);
    let json = serde_json::to_string(&res).map_err(error::ErrorInternalServerError)?;

    Ok(HttpResponse::Ok()
        .content_type("application/json")
        .body(json))
}

pub(super) fn playground(opt: web::Data<Opt>) -> HttpResponse {
    let html = playground_source(&format!("http://{}:{}/graphql", opt.domain, opt.port));
    HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body(html)
}
