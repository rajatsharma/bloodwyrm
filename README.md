# Ceraxes

> 🐉 Toolbelt for generating Graphql APIs with Rust

## Introduction

Ceraxes is a command line tool which helps you bootstrap and generate schema and glue code for a Graphql API. It is based on [async-graphql](https://github.com/async-graphql/async-graphql) and [seaorm](https://github.com/SeaQL/sea-orm). It can generate database entities, migrations and graphql schema using one single command.

## Usage

- Initialise project 
  - `ceraxes init PROJECT_NAME`

```sh
ceraxes init my_gql_server
```

- Create DB entities and associated Graphql Schema 
  - `ceraxes gen-model MODEL_NAME MODEL_NAME_PLURAL [FIELD...]`

Use `!` to mark column(field) as required

```sh
ceraxes gen-model blog blogs name:string! desc:string author:string!
```
