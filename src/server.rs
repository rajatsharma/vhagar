use actix_web::middleware::Logger;
use actix_web::{web, App, HttpServer};

pub async fn main() -> std::io::Result<()> {
    // Gets enviroment variables from `.env.example`
    dotenv::dotenv().ok();

    // Initiates error logger
    env_logger::init();

    // Sets options to enviroment variables
    let opt = {
        use structopt::StructOpt;
        crate::cli_args::Opt::from_args()
    };

    // Database
    let pool = crate::database::pool::establish_connection(opt.clone());
    let schema = std::sync::Arc::new(crate::graphql::model::create_schema());

    // Server port
    let port = opt.port;

    // Server
    let server = HttpServer::new(move || {
        // prevents double Arc
        let schema: web::Data<crate::graphql::model::Schema> = schema.clone().into();

        App::new()
            // Database
            .data(pool.clone())
            .app_data(schema)
            // Options
            .data(opt.clone())
            // Error logging
            .wrap(Logger::default())
            .configure(crate::graphql::route)
    })
    // Running at `format!("{}:{}",port,"0.0.0.0")`
    .bind(("0.0.0.0", port))
    .unwrap()
    // Starts server
    .run();

    eprintln!("Listening on 0.0.0.0:{}", port);

    // Awaiting server to exit
    server.await
}
