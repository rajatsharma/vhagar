# Kensai

> Rapid Graphql based API development using Rust

## Usage

### Commands

- `kensai init`

Initialises Rust project with:

- Juniper Graphql
- Actix Web Server

Run these commands after project initialises

```sh
diesel setup
```

Make sure env variable `DATABASE_URL` is set.

- `kensai generate <model_name>`

Creates new model and associated operations

Run these commands after done

```sh
diesel migration generate `create_<model_name>`
diesel migration run
```
