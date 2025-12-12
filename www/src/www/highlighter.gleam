/// Syntax highlighting using Shiki via FFI
/// Process HTML string, replacing code blocks with syntax-highlighted versions
@external(javascript, "../highlighter.ffi.mjs", "highlightHtml")
pub fn highlight_html(html: String) -> String
