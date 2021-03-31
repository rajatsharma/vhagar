#[macro_use]
extern crate diesel;
#[macro_use]
extern crate serde_derive;

mod cli_args;
mod database;
mod errors;
mod graphql;
mod schema;
pub mod server;
mod todo;
