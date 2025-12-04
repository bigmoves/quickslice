# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Add isNull filter support for ref fields in where clauses (5d9f426)
- Improve GraphQL type generation for lexicons (926d449)
- Add statusphere HTML example and viewer query (d94e41a)
- Add OAuth scope validation and client type support (dc1930a)
- Add Model Context Protocol (MCP) server (98e54d5)
- Refactor admin DID handling and add Settings.adminDids field (2484ea1)
- Migrate environment variables to database config table (cbb7b25)
- Add PLC_DIRECTORY_URL env var override for bootstrap (32aec91)
- Handle OAuth errors with proper redirects (497c5e4)
- Sync actor records on first login (47f2d32)

### Fixed
- Resolve strongRef refs in nested object types (434a89f)
- Resolve nested refs within others object types (70d972d)
- Show reset alert in danger zone section of settings (7bedfb3)
- Correct test expectation for invalid scope error handling (66d69e8)

### Changed
- Implement nested forward join resolution for strongRef fields (3304ad9)
- Remove /example folder, move docker-compose to root (2a67762)
- Move docs/plans to dev-docs/plans (a9d7648)
- Update settings (ef37374)

### Documentation
- Remove deprecated env vars from deployment guide (7da5864)
