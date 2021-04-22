use super::model::Context;
use diesel::pg::PgConnection;
use juniper::FieldResult;

pub(crate) struct Mutation;

#[juniper::object(Context = Context)]
impl Mutation {
    //Replace me
}
