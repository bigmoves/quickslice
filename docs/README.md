# Quickslice

Quickslice is an AppView for AT Protocol applications. Import your Lexicon schemas and you get a GraphQL API with OAuth authentication, real-time sync from the network, and joins across record types.

## The Problem

Building an AppView from scratch means writing a lot of infrastructure code:

- Jetstream connection and event handling
- Record ingestion and validation
- Database schema design and normalization
- API endpoints for querying and writing data
- OAuth session management and PDS writes
- Efficient batching when resolving related records

This adds up before you write any application logic.

## What Quickslice Does

Quickslice handles all of that automatically:

- **Connects to Jetstream** and tracks the record types defined in your Lexicons
- **Indexes records** into a normalized database with proper foreign key relationships
- **Generates GraphQL** queries, mutations, and subscriptions from your Lexicon definitions
- **Handles OAuth** and writes records back to the user's PDS
- **Enables joins** by DID, URI, or strong reference, so you can query a status and its author's profile in one request

## When to Use It

Use Quickslice when you're building any application that needs to aggregate and query AT Protocol data. If your app reads from the network and needs a backend, Quickslice makes this easy.

## Next Steps

[Build Statusphere with Quickslice](tutorial.md): A hands-on tutorial showing what Quickslice handles for you
