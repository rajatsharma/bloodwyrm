use entity::async_graphql;
/*Ceraxes Query Mod Marker*/

// Add your other ones here to create a unified Query object
// e.x. Query(NoteQuery, OtherQuery, OtherOtherQuery)
#[derive(async_graphql::MergedObject, Default)]
pub struct Query(/*Ceraxes Query Marker*/);
