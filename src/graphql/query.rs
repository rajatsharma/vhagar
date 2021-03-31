use super::model::Context;
use diesel::pg::PgConnection;
use juniper::FieldResult;

pub struct Query;

#[juniper::object(Context = Context)]
impl Query {
    fn todos(context: &Context) -> FieldResult<Vec<crate::todo::Todo>> {
        let conn: &PgConnection = &context.db;
        crate::todo::Todos::all(conn)
    }

    //Replace me
}
