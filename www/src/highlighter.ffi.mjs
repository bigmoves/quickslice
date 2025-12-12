// Shiki syntax highlighting FFI for Gleam
import { createHighlighter } from 'shiki'
import { readFileSync } from 'fs'
import { join } from 'path'

// Theme path relative to working directory (www/)
const themePath = join(process.cwd(), 'static', 'quickslice-theme.json')
const theme = JSON.parse(readFileSync(themePath, 'utf-8'))

// Initialize highlighter at module load (top-level await)
const highlighter = await createHighlighter({
  themes: [theme],
  langs: ['graphql', 'json', 'javascript', 'typescript', 'bash', 'shell', 'toml', 'yaml', 'html', 'css', 'sql', 'elixir', 'gleam', 'text'],
})

const langMap = {
  'js': 'javascript',
  'ts': 'typescript',
  'sh': 'bash',
  'yml': 'yaml',
}

// Regex to match <pre><code class="language-xxx">...</code></pre>
const codeBlockRegex = /<pre><code class="language-(\w+)">([\s\S]*?)<\/code><\/pre>/g

export function highlightHtml(html) {
  return html.replace(codeBlockRegex, (match, lang, code) => {
    const resolvedLang = langMap[lang] || lang

    // Decode HTML entities
    const decoded = code
      .replace(/&lt;/g, '<')
      .replace(/&gt;/g, '>')
      .replace(/&amp;/g, '&')
      .replace(/&quot;/g, '"')
      .replace(/&#39;/g, "'")

    try {
      return highlighter.codeToHtml(decoded, {
        lang: resolvedLang,
        theme: 'quickslice',
      })
    } catch (e) {
      // Language not supported, return original
      return match
    }
  })
}
