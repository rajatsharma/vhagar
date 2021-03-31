use crate::schema::todos;
use diesel::pg::PgConnection;
use diesel::ExpressionMethods;
use diesel::QueryDsl;
use diesel::RunQueryDsl;
use juniper::FieldResult;

#[derive(Queryable, juniper::GraphQLObject)]
pub struct Todo {
    pub id: i32,
    pub task: String,
    pub done: Option<bool>,
}

#[derive(Insertable, juniper::GraphQLInputObject)]
#[table_name = "todos"]
pub struct NewTodo {
    pub task: String,
    pub done: Option<bool>,
}

pub struct Todos;

impl Todos {
    pub fn all(conn: &PgConnection) -> FieldResult<Vec<Todo>> {
        todos::table.load::<Todo>(conn).map_err(|err| err.into())
    }
    pub fn create(conn: &PgConnection, new_todo: NewTodo) -> FieldResult<Todo> {
        diesel::insert_into(todos::table)
            .values(&new_todo)
            .get_result(conn)
            .map_err(|err| err.into())
    }
    pub fn delete(conn: &PgConnection, id: i32) -> FieldResult<Todo> {
        diesel::delete(todos::table.filter(todos::id.eq(id)))
            .get_result(conn)
            .map_err(|err| err.into())
    }
}
