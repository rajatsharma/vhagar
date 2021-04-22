use super::model::Context;
use diesel::pg::PgConnection;
use juniper::FieldResult;

pub(crate) struct Query;

#[juniper::object(Context = Context)]
impl Query {
    //Replace me
}
