//// Public API for lexicon_graphql package
////
//// This module re-exports the main public API functions and types.
//// For more specialized usage, import specific submodules:
//// - lexicon_graphql/schema/builder - Basic schema building
//// - lexicon_graphql/schema/database - Database-backed schema building
//// - lexicon_graphql/query/dataloader - Batching and pagination types
//// - lexicon_graphql/mutation/builder - Mutation operations
//// - lexicon_graphql/input/* - Input types (where, aggregate, connection)
//// - lexicon_graphql/output/* - Output types (aggregate results)
//// - lexicon_graphql/scalar/* - Custom scalar types (blob)

// Import statements for re-exports
import gleam/option.{type Option}
import lexicon_graphql/internal/lexicon/parser as lexicon_parser
import lexicon_graphql/mutation/builder as mutation_builder
import lexicon_graphql/query/dataloader
import lexicon_graphql/schema/builder as schema_builder
import lexicon_graphql/schema/database as db_schema_builder
import lexicon_graphql/types
import swell/schema

// Re-export core types
pub type Lexicon =
  types.Lexicon

pub type Defs =
  types.Defs

pub type Def =
  types.Def

pub type RecordDef =
  types.RecordDef

pub type ObjectDef =
  types.ObjectDef

pub type Property =
  types.Property

// Re-export fetcher types
pub type ViewerFetcher =
  db_schema_builder.ViewerFetcher

pub type NotificationFetcher =
  db_schema_builder.NotificationFetcher

// Re-export main schema building functions
pub fn build_schema(lexicons: List(Lexicon)) {
  schema_builder.build_schema(lexicons)
}

pub fn build_schema_with_subscriptions(
  lexicons: List(Lexicon),
  fetcher: db_schema_builder.RecordFetcher,
  batch_fetcher: Option(dataloader.BatchFetcher),
  paginated_batch_fetcher: Option(dataloader.PaginatedBatchFetcher),
  create_factory: Option(mutation_builder.ResolverFactory),
  update_factory: Option(mutation_builder.ResolverFactory),
  delete_factory: Option(mutation_builder.ResolverFactory),
  upload_blob_factory: Option(mutation_builder.UploadBlobResolverFactory),
  aggregate_fetcher: Option(db_schema_builder.AggregateFetcher),
  viewer_fetcher: Option(ViewerFetcher),
  notification_fetcher: Option(NotificationFetcher),
  viewer_state_fetcher: Option(dataloader.ViewerStateFetcher),
  labels_fetcher: Option(db_schema_builder.LabelsFetcher),
  custom_mutation_fields: Option(List(schema.Field)),
  custom_query_fields: Option(List(schema.Field)),
) {
  db_schema_builder.build_schema_with_subscriptions(
    lexicons,
    fetcher,
    batch_fetcher,
    paginated_batch_fetcher,
    create_factory,
    update_factory,
    delete_factory,
    upload_blob_factory,
    aggregate_fetcher,
    viewer_fetcher,
    notification_fetcher,
    viewer_state_fetcher,
    labels_fetcher,
    custom_mutation_fields,
    custom_query_fields,
  )
}

// Re-export lexicon parser
pub fn parse_lexicon(json_str: String) {
  lexicon_parser.parse_lexicon(json_str)
}
