use super::mutation::Mutation;
use super::query::Query;
use crate::cli_args::Opt;
use crate::database::PooledConnection;
use juniper::Context as JuniperContext;
use std::sync::Arc;

#[derive(Clone)]
pub(crate) struct Context {
    pub opt: Opt,
    pub db: Arc<PooledConnection>,
}

impl JuniperContext for Context {}

impl Context {
    pub fn new(pool: PooledConnection, opt: Opt) -> Self {
        Self {
            opt,
            db: Arc::new(pool),
        }
    }
}

pub(crate) type Schema = juniper::RootNode<'static, Query, Mutation>;

pub(crate) fn create_schema() -> Schema {
    Schema::new(Query {}, Mutation {})
}
