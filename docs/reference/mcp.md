# MCP Server

Quickslice provides an MCP (Model Context Protocol) server that lets AI assistants query ATProto data directly.

## Endpoint

Every Quickslice instance exposes MCP at `{EXTERNAL_BASE_URL}/mcp`. For example:

```
https://xyzstatusphere.slices.network/mcp
```

## Setup

### Claude Code

```bash
claude mcp add --transport http quickslice https://xyzstatusphere.slices.network/mcp
```

### Other MCP Clients

Point any MCP-compatible client at the `/mcp` endpoint using HTTP transport.

## Available Tools

| Tool | Description |
|------|-------------|
| `list_lexicons` | List all registered lexicons |
| `get_lexicon` | Get full lexicon definition by NSID |
| `list_queries` | List available GraphQL queries |
| `get_oauth_info` | Get OAuth flows, scopes, and endpoints |
| `get_server_capabilities` | Get server version and features |
| `introspect_schema` | Get full GraphQL schema |
| `execute_query` | Execute a GraphQL query |

## Example Prompts

Once connected, you can ask things like:

- "What lexicons are registered?"
- "Show me the schema for xyz.statusphere.status"
- "Query the latest 10 statusphere statuses"
- "What GraphQL queries are available?"
- "What OAuth scopes does this server support?"
