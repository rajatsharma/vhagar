use super::model::Context;
use diesel::pg::PgConnection;
use juniper::FieldResult;

pub(crate) struct Mutation;

#[juniper::object(Context = Context)]
impl Mutation {
    fn create_todo(
        context: &Context,
        new_todo: crate::todo::NewTodo,
    ) -> FieldResult<crate::todo::Todo> {
        let conn: &PgConnection = &context.db;
        crate::todo::Todos::create(conn, new_todo)
    }
    fn delete_todo(context: &Context, id: i32) -> FieldResult<crate::todo::Todo> {
        let conn: &PgConnection = &context.db;
        crate::todo::Todos::delete(conn, id)
    }

    //Replace me
}
