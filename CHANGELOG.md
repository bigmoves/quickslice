# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.17.5

### Fixed
- Publish pubsub events after GraphQL mutations for immediate WebSocket subscription updates

## quickslice-client-js v0.3.0

### Added
- Add storage namespace isolation for multi-app support
  - Storage keys prefixed with 8-char SHA-256 hash of clientId
  - IndexedDB database name includes namespace
  - Lock keys include namespace

### Breaking Changes
- Existing users will need to re-login once after update

## quickslice-client-js v0.2.0

### Added
- Add `scope` parameter to `QuicksliceClientOptions` for setting default OAuth scope
- Add `scope` option to `loginWithRedirect()` for per-login scope override

## v0.17.4

### Fixed
- Fix "Invalid refresh token" error caused by session iteration drift after ATP token refresh

## v0.17.3

### Fixed
- Add `sub` claim to OAuth token response for client SDK user identification

## v0.17.2

### Fixed
- Fix reverse joins not available through forward join *Resolved fields (swell 2.1.2)

## v0.17.1

### Added
- Add ARM64 (linux/arm64) Docker image support

## v0.17.0

### Added
- Add redirectUri option to QuicksliceClient
- Make lexicon import declarative (wipe-and-replace)
- Add multi-database support (PostgreSQL and SQLite)
  - Add unified Executor type for database abstraction
  - Add SQLite executor with PRAGMA setup
  - Add PostgreSQL executor with pog driver
  - Add unified connection module with DATABASE_URL detection
- Add dbmate schema migrations for SQLite and PostgreSQL
- Add Makefile for database operations
- Add GIN index on record.json for efficient JSONB queries
- Add dbmate and auto-migrations to Docker build
- Add docker entrypoint script for auto-migrations

### Fixed
- Complete AT Protocol token refresh implementation
- Transform BlobInput to AT Protocol format in create/update mutations
- Use all lexicons for ref validation in create/update mutations
- Fix quickslice client exports

### Changed
- Extract car/ into standalone atproto_car package
- Reorganize lexicon GraphQL into modular structure
- Reorganize admin GraphQL schema into modular structure
- Reorganize database helpers into proper layers
- Move graphql_ws into graphql/ws module
- Split settings page into section modules
- Remove unused dependencies, handlers, and CLI commands
- Update honk to v1.2
- Migrate all repositories to Executor pattern (config, OAuth, records, pagination)
- Update where_clause to support database dialects
- Remove Gleam-based migration system in favor of dbmate
- Update server startup to use Executor
- Cache Gleam build in CI to speed up native dependency compilation

## v0.16.0

### Added
- Add secure public OAuth flow with DPoP and quickslice-client-js SDK

### Fixed
- Pass OAuth scopes through without filtering in client metadata

### Changed
- Update docker-compose
- Add editorconfig and format examples HTML

## v0.15.1

### Fixed
- Pass OAuth scopes through without filtering in client metadata

## v0.15.0

### Added
- Add isNull filter support for ref fields in where clauses
- Improve GraphQL type generation for lexicons
- Add statusphere HTML example and viewer query
- Add OAuth scope validation and client type support
- Add Model Context Protocol (MCP) server
- Refactor admin DID handling and add Settings.adminDids field
- Migrate environment variables to database config table
- Add PLC_DIRECTORY_URL env var override for bootstrap
- Handle OAuth errors with proper redirects
- Sync actor records on first login

### Fixed
- Encode non-UTF-8 binary data as $bytes in JSON
- Resolve strongRef refs in nested object types
- Resolve nested refs within others object types
- Show reset alert in danger zone section of settings
- Correct test expectation for invalid scope error handling

### Changed
- Implement nested forward join resolution for strongRef fields
- Remove /example folder, move docker-compose to root
- Move docs/plans to dev-docs/plans
- Update settings

### Documentation
- Remove deprecated env vars from deployment guide
