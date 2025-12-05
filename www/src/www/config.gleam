/// Configuration for the docs site

/// Represents a documentation page
pub type DocPage {
  DocPage(
    /// Filename without extension (e.g., "queries")
    slug: String,
    /// URL path (e.g., "/queries")
    path: String,
    /// Display title for navigation
    title: String,
    /// Markdown content (loaded from file)
    content: String,
  )
}

/// Manual ordering of documentation pages
/// Format: #(filename, path, nav_title)
pub const page_order: List(#(String, String, String)) = [
  #("README.md", "/", "Getting Started"),
  #("authentication.md", "/authentication", "Authentication"),
  #("queries.md", "/queries", "Queries"),
  #("mutations.md", "/mutations", "Mutations"),
  #("joins.md", "/joins", "Joins"),
  #("aggregations.md", "/aggregations", "Aggregations"),
  #("subscriptions.md", "/subscriptions", "Subscriptions"),
  #("blobs.md", "/blobs", "Blobs"),
  #("variables.md", "/variables", "Variables"),
  #("deployment.md", "/deployment", "Deployment"),
  #("mcp.md", "/mcp", "MCP"),
]

/// Path to the docs directory (relative to project root)
pub const docs_dir: String = "../docs"

/// Output directory for generated site
pub const out_dir: String = "./priv"
