table! {
    todos (id) {
        id -> Int4,
        task -> Varchar,
        done -> Nullable<Bool>,
    }
}

table! {
  contents (id) {
    id -> Int4,
      name -> Varchar,
      uid -> Uuid,
  }
}

//Replace me
