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

This adds up before you write any application logic. There are also a lot of patterns that are more or less the same that every AppView will need to implement.

## What Quickslice Does

Quickslice handles all of that automatically:

- **Connects to Jetstream** and tracks the record types defined in your Lexicons
- **Indexes** relevant records into a database (SQLite, more soon)
- **Generates GraphQL** queries, mutations, and subscriptions from your Lexicon definitions
- **Handles OAuth** and writes records back to the user's PDS
- **Enables joins** by DID, URI, or strong reference, so you can query a status and its author's profile in one request

## When to Use It

- You've setup tried and failed to build an AppView from scratch
- You just want to tinker with Lexicon data structures and build a quick prototype
- You've banged your head against OAuth for too long
- You just want to ship your AppView already
- The *sky* is the limit!

## Next Steps

[Build Statusphere with Quickslice](tutorial.md): A hands-on tutorial showing what Quickslice handles for you
