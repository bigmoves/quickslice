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
    /// Sidebar group name
    group: String,
    /// Markdown content (loaded from file)
    content: String,
  )
}

/// Navigation group for sidebar
pub type NavGroup {
  NavGroup(name: String, pages: List(#(String, String, String)))
}

/// Sidebar navigation structure
/// Each group contains: #(filename, path, nav_title)
pub const navigation: List(NavGroup) = [
  NavGroup(
    "Getting Started",
    [
      #("README.md", "/", "Introduction"),
      #("quickstart.md", "/quickstart", "Quickstart"),
      #("building-apps.md", "/building-apps", "Building Apps"),
      #("authentication.md", "/authentication", "Authentication"),
    ],
  ),
  NavGroup(
    "GraphQL",
    [
      #("queries.md", "/queries", "Queries"),
      #("mutations.md", "/mutations", "Mutations"),
      #("joins.md", "/joins", "Joins"),
      #("aggregations.md", "/aggregations", "Aggregations"),
      #("subscriptions.md", "/subscriptions", "Subscriptions"),
      #("blobs.md", "/blobs", "Blobs"),
      #("variables.md", "/variables", "Variables"),
    ],
  ),
  NavGroup(
    "Other",
    [
      #("deployment.md", "/deployment", "Deployment"),
      #("mcp.md", "/mcp", "MCP"),
    ],
  ),
]

/// Path to the docs directory (relative to project root)
pub const docs_dir: String = "../docs"

/// Output directory for generated site
pub const out_dir: String = "./priv"

/// Static assets directory
pub const static_dir: String = "./static"

/// Base URL for the site
pub const base_url: String = "https://quickslice.slices.network"

/// Get the URL for the OG image (single image for all pages)
pub fn og_image_url(_page: DocPage) -> String {
  base_url <> "/og/default.webp"
}
