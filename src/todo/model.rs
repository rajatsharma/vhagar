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
    pub done: bool,
}

#[derive(Insertable, juniper::GraphQLInputObject)]
#[table_name = "todos"]
pub struct NewTodo {
    pub task: String,
    pub done: Option<bool>,
}

pub struct Todos;

impl Todos {
    pub fn all_todos(conn: &PgConnection) -> FieldResult<Vec<Todo>> {
        Ok(todos::table.load::<Todo>(conn)?)
    }
    pub fn create_todo(conn: &PgConnection, new_todo: NewTodo) -> FieldResult<Todo> {
        Ok(diesel::insert_into(todos::table)
            .values(&new_todo)
            .get_result(conn)?)
    }
    pub fn delete_todo(conn: &PgConnection, id: i32) -> FieldResult<Todo> {
        Ok(diesel::delete(todos::table.filter(todos::id.eq(id))).get_result(conn)?)
    }
}
