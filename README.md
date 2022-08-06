# Vhagar

## Introduction

Vhagar is a command line tool which helps you bootstrap and maintain a Graphql Api. It is based on gqlgen and gorm. It generates structs, graphql schema and populates boilerplate code with a single command and lets you work on the main logic of the graphql resolvers.

## Usage

- Initialise project 
  - `vhagar init PROJECT_NAME`

```sh
vhagar init my_gql_server
```

- Create DB entities and associated Graphql Schema 
  - `vhagar gen-model MODEL_NAME MODEL_NAME_PLURAL [FIELD...]`

```sh
vhagar gen-model blog blogs name:String! desc:String! author:String!
```

- Regenerate Graphql resolvers after updating Graphql schema 
  - `vhagar regen`

```sh
vhagar regen
```


> Vhagar, the greatest of the Targaryen dragons since the passing of Balerion the Black Dread, had counted one hundred eighty-one years upon the earth. Thus passed the last living creature from the days of Aegon's Conquest, as dusk and darkness swallowed Black Harren's accursed seat. — **The Princess and the Queen, or, the Blacks and the Greens**, *A Song of Ice and Fire*
