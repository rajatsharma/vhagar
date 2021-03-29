#[macro_use]
extern crate diesel;
#[macro_use]
extern crate serde_derive;

pub mod cli_args;
pub mod database;
mod errors;
pub mod graphql;
mod schema;
mod todo;
