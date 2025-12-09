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
      #("tutorial.md", "/tutorial", "Tutorial"),
    ],
  ),
  NavGroup(
    "Guides",
    [
      #("guides/queries.md", "/guides/queries", "Queries"),
      #("guides/joins.md", "/guides/joins", "Joins"),
      #("guides/mutations.md", "/guides/mutations", "Mutations"),
      #("guides/authentication.md", "/guides/authentication", "Authentication"),
      #("guides/deployment.md", "/guides/deployment", "Deployment"),
      #("guides/patterns.md", "/guides/patterns", "Patterns"),
      #(
        "guides/troubleshooting.md",
        "/guides/troubleshooting",
        "Troubleshooting",
      ),
    ],
  ),
  NavGroup(
    "Reference",
    [
      #("reference/aggregations.md", "/reference/aggregations", "Aggregations"),
      #(
        "reference/subscriptions.md",
        "/reference/subscriptions",
        "Subscriptions",
      ),
      #("reference/blobs.md", "/reference/blobs", "Blobs"),
      #("reference/variables.md", "/reference/variables", "Variables"),
      #("reference/mcp.md", "/reference/mcp", "MCP"),
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
