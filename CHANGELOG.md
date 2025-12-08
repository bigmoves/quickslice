# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
