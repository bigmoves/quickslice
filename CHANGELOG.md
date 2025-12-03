# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Add statusphere HTML example and viewer query (d94e41a)
- Add comprehensive OAuth scope validation and client type support (dc1930a)
- Add Model Context Protocol (MCP) server (5a575a8)
- Refactor admin DID handling and add Settings.adminDids field (2484ea1)
- Migrate environment variables to database config table (cbb7b25)
- Add PLC_DIRECTORY_URL env var override for bootstrap (32aec91)
- Handle OAuth errors with proper redirects (497c5e4)
- Sync actor records on first login (47f2d32)

### Fixed
- Correct test expectation for invalid scope error handling (66d69e8)

### Changed
- Remove /example folder, move docker-compose to root (2a67762)
- Move docs/plans to dev-docs/plans (a9d7648)
- Update settings (ef37374)

### Documentation
- Add authentication guide for app developers (234bf75)
- Add OAuth scopes implementation plan (6fd8716)
- Add admin onboarding implementation plan (5a575a8)
- Remove deprecated env vars from deployment guide (7da5864)
