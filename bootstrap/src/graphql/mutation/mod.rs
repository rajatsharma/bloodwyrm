use ::async_graphql;
/*Ceraxes Mutation Mod Marker*/

// Add your other ones here to create a unified Mutation object
// e.x. Mutation(NoteMutation, OtherMutation, OtherOtherMutation)
#[derive(async_graphql::MergedObject, Default)]
pub struct Mutation(/*Ceraxes Mutation Marker*/);
