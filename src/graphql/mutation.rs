use super::model::Context;
use crate::todo;
use diesel::pg::PgConnection;
use juniper::FieldResult;

pub struct Mutation;

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
