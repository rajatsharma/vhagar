-- Your SQL goes here

CREATE TABLE todos (
  id SERIAL PRIMARY KEY,
  task VARCHAR NOT NULL,
  done BOOLEAN NOT NULL DEFAULT 'f'
)
