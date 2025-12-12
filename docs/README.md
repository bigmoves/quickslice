# Quickslice

Quickslice is a quick way to spin up an [AppView](https://atproto.com/guides/glossary#app-view) for AT Protocol applications. Import your Lexicon schemas and you get a GraphQL API with OAuth authentication, real-time sync from the network, and joins across record types without setting up a database or writing any backend code.

## The Problem

Building an AppView from scratch means writing a lot of infrastructure code:

- Jetstream connection and event handling
- Record ingestion and validation
- Database schema design and normalization
- XRPC API endpoints for querying and writing data
- OAuth session management and PDS writes
- Efficient batching when resolving related records

This adds up before you write any application logic.

## What Quickslice Does

Quickslice handles all of that automatically:

- **Connects to Jetstream** and tracks the record types defined in your Lexicons
- **Indexes** relevant records into a database (SQLite; Postgres coming)
- **Generates GraphQL** queries, mutations, and subscriptions from your Lexicon definitions
- **Handles OAuth** and writes records back to the user's PDS
- **Enables joins** by DID, URI, or strong reference, so you can query a status and its author's profile in one request

## When to Use It

- You want to skip the AppView boilerplate
- You want to prototype Lexicon data structures quickly
- You want OAuth handled for you
- You want to ship your AppView now

## Next Steps

[Build Statusphere with Quickslice](tutorial.md): A hands-on tutorial showing what Quickslice handles for you
