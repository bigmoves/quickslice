// Lightweight syntax highlighter for GraphQL, JSON, and JavaScript
(function() {
  const graphqlTokens = [
    [/#.*$/gm, 'comment'],
    [/"(?:[^"\\]|\\.)*"/g, 'string'],
    [/\b(query|mutation|subscription|fragment|on|type|interface|union|enum|scalar|input|extend|directive|schema|implements)\b/g, 'keyword'],
    [/\b(String|Int|Float|Boolean|ID)\b/g, 'type'],
    [/@\w+/g, 'directive'],
    [/\$\w+/g, 'variable'],
    [/\b(\w+)(?=\s*:)/g, 'property'],
    [/\b([A-Z]\w*)\b/g, 'type'],
    [/\b\d+\.?\d*\b/g, 'number'],
    [/[{}()\[\]:,!=|&]/g, 'punctuation'],
  ];

  const jsonTokens = [
    [/"(?:[^"\\]|\\.)*"(?=\s*:)/g, 'property'],
    [/"(?:[^"\\]|\\.)*"/g, 'string'],
    [/\b(true|false|null)\b/g, 'keyword'],
    [/-?\b\d+\.?\d*(?:[eE][+-]?\d+)?\b/g, 'number'],
    [/[{}()\[\]:,]/g, 'punctuation'],
  ];

  const bashTokens = [
    // Comments - but not shebang
    [/(^|[^"{\\$])#.*/gm, 'comment'],
    // Strings
    [/"(?:[^"\\]|\\.)*"/g, 'string'],
    [/'[^']*'/g, 'string'],
    [/\$'(?:[^'\\]|\\.)*'/g, 'string'],
    // Variables
    [/\$\(\([\s\S]+?\)\)/g, 'variable'],
    [/\$\([^)]+\)/g, 'variable'],
    [/\$\{[^}]+\}/g, 'variable'],
    [/\$(?:\w+|[#?*!@$])/g, 'variable'],
    // Keywords
    [/\b(case|do|done|elif|else|esac|fi|for|function|if|in|select|then|until|while|return|exit|break|continue|local|export|readonly|declare|unset|shift|source)\b/g, 'keyword'],
    // Common commands/builtins
    [/\b(echo|printf|cd|pwd|ls|cat|cp|mv|rm|mkdir|rmdir|touch|chmod|chown|grep|sed|awk|find|xargs|sort|uniq|head|tail|wc|curl|wget|git|npm|npx|yarn|pnpm|node|python|pip|docker|kubectl|sudo|apt|brew|make|test)\b/g, 'function'],
    // Numbers - only standalone, not in URLs/paths
    [/(?<![:/\w])\b\d+\b(?![:/\w])/g, 'number'],
    // Operators and punctuation
    [/[|&;><(){}[\]]/g, 'punctuation'],
  ];

  const jsTokens = [
    // Strings MUST come first to prevent // inside URLs from being matched as comments
    // Regular strings
    [/"(?:[^"\\]|\\.)*"/g, 'string'],
    [/'(?:[^'\\]|\\.)*'/g, 'string'],
    // Template literals with interpolation support
    [/`(?:[^`\\]|\\.|\$\{[^}]*\})*`/g, 'string'],
    // Comments (after strings)
    [/\/\/.*$/gm, 'comment'],
    [/\/\*[\s\S]*?\*\//g, 'comment'],
    [/\b(const|let|var|function|return|if|else|for|while|do|switch|case|break|continue|try|catch|finally|throw|new|delete|typeof|instanceof|in|of|class|extends|super|import|export|default|from|as|async|await|yield|this|static|get|set)\b/g, 'keyword'],
    [/\b(true|false|null|undefined|NaN|Infinity)\b/g, 'keyword'],
    [/\b(Array|Object|String|Number|Boolean|Function|Symbol|Map|Set|WeakMap|WeakSet|Promise|Date|RegExp|Error|JSON|Math|console|window|document|location)\b/g, 'type'],
    [/\b(\w+)(?=\s*\()/g, 'function'],
    [/-?\b\d+\.?\d*(?:[eE][+-]?\d+)?\b/g, 'number'],
    [/[{}()\[\]:;,=+\-*/%<>!&|?\.]/g, 'punctuation'],
  ];

  function escapeHtml(text) {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;');
  }

  function highlight(code, tokens) {
    const highlighted = new Array(code.length).fill(false);
    const spans = [];

    for (const [regex, className] of tokens) {
      regex.lastIndex = 0;
      let match;
      while ((match = regex.exec(code)) !== null) {
        const start = match.index;
        const end = start + match[0].length;

        // Check if ANY position in this match is already highlighted
        // This prevents patterns from matching inside already-highlighted regions
        let overlap = false;
        for (let i = start; i < end; i++) {
          if (highlighted[i]) {
            overlap = true;
            break;
          }
        }
        if (overlap) {
          continue;
        }

        spans.push({ start, end, className, text: match[0] });
        for (let i = start; i < end; i++) {
          highlighted[i] = true;
        }
      }
    }

    spans.sort((a, b) => a.start - b.start);

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
    // GraphQL
    const graphqlBlocks = document.querySelectorAll('pre code.language-graphql');
    for (const block of graphqlBlocks) {
      block.innerHTML = highlight(block.textContent, graphqlTokens);
    }

    // JSON
    const jsonBlocks = document.querySelectorAll('pre code.language-json');
    for (const block of jsonBlocks) {
      block.innerHTML = highlight(block.textContent, jsonTokens);
    }

    // JavaScript / JS
    const jsBlocks = document.querySelectorAll('pre code.language-javascript, pre code.language-js');
    for (const block of jsBlocks) {
      block.innerHTML = highlight(block.textContent, jsTokens);
    }

    // Bash / Shell
    const bashBlocks = document.querySelectorAll('pre code.language-bash, pre code.language-shell, pre code.language-sh');
    for (const block of bashBlocks) {
      block.innerHTML = highlight(block.textContent, bashTokens);
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
