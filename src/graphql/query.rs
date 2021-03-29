use super::model::Context;
use crate::todo;
use diesel::pg::PgConnection;
use juniper::FieldResult;

pub struct Query;

#[juniper::object(Context = Context)]
impl Query {
    fn todos(context: &Context) -> FieldResult<Vec<todo::Todo>> {
        let conn: &PgConnection = &context.db;
        todo::Todos::all_todos(conn)
    }
}
