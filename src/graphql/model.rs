use crate::database::PooledConnection;
use crate::{cli_args::Opt, todo};
use diesel::pg::PgConnection;
use juniper::Context as JuniperContext;
use juniper::FieldResult;
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

pub(crate) struct Query;

#[juniper::object(Context = Context)]
impl Query {
    fn todos(context: &Context) -> FieldResult<Vec<todo::Todo>> {
        let conn: &PgConnection = &context.db;
        todo::Todos::all_todos(conn)
    }
}

pub(crate) struct Mutation;

#[juniper::object(Context = Context)]
impl Mutation {
    fn create_todo(context: &Context, new_todo: todo::NewTodo) -> FieldResult<todo::Todo> {
        let conn: &PgConnection = &context.db;
        todo::Todos::create_todo(conn, new_todo)
    }
    fn delete_todo(context: &Context, id: i32) -> FieldResult<todo::Todo> {
        let conn: &PgConnection = &context.db;
        todo::Todos::delete_todo(conn, id)
    }
}

pub(crate) type Schema = juniper::RootNode<'static, Query, Mutation>;

pub(crate) fn create_schema() -> Schema {
    Schema::new(Query {}, Mutation {})
}
