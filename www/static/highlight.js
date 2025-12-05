// Lightweight GraphQL syntax highlighter
(function() {
  const tokens = [
    // Comments
    [/#.*$/gm, 'comment'],
    // Strings
    [/"(?:[^"\\]|\\.)*"/g, 'string'],
    // Keywords
    [/\b(query|mutation|subscription|fragment|on|type|interface|union|enum|scalar|input|extend|directive|schema|implements)\b/g, 'keyword'],
    // Built-in types
    [/\b(String|Int|Float|Boolean|ID)\b/g, 'type'],
    // Directives
    [/@\w+/g, 'directive'],
    // Variables
    [/\$\w+/g, 'variable'],
    // Field arguments / input fields
    [/\b(\w+)(?=\s*:)/g, 'property'],
    // Type names (capitalized words not caught above)
    [/\b([A-Z]\w*)\b/g, 'type'],
    // Numbers
    [/\b\d+\.?\d*\b/g, 'number'],
    // Punctuation
    [/[{}()\[\]:,!=|&]/g, 'punctuation'],
  ];

  function escapeHtml(text) {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;');
  }

  function highlight(code) {
    // Track which character positions are already highlighted
    const highlighted = new Array(code.length).fill(false);
    const spans = [];

    for (const [regex, className] of tokens) {
      regex.lastIndex = 0;
      let match;
      while ((match = regex.exec(code)) !== null) {
        const start = match.index;
        const end = start + match[0].length;

        // Check if any part of this match is already highlighted
        let alreadyHighlighted = false;
        for (let i = start; i < end; i++) {
          if (highlighted[i]) {
            alreadyHighlighted = true;
            break;
          }
        }

        if (!alreadyHighlighted) {
          spans.push({ start, end, className, text: match[0] });
          for (let i = start; i < end; i++) {
            highlighted[i] = true;
          }
        }
      }
    }

    // Sort spans by start position
    spans.sort((a, b) => a.start - b.start);

    // Build result
    let result = '';
    let pos = 0;
    for (const span of spans) {
      if (span.start > pos) {
        result += escapeHtml(code.slice(pos, span.start));
      }
      result += `<span class="hl-${span.className}">${escapeHtml(span.text)}</span>`;
      pos = span.end;
    }
    if (pos < code.length) {
      result += escapeHtml(code.slice(pos));
    }

    return result;
  }

  function init() {
    const blocks = document.querySelectorAll('pre code.language-graphql');
    for (const block of blocks) {
      block.innerHTML = highlight(block.textContent);
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
