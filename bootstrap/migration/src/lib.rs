pub use sea_orm_migration::prelude::*;
/*Ceraxed Migrator Mod Marker*/
pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![/*Ceraxed Migrator Marker*/]
    }
}
