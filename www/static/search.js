// Search functionality for docs site
(function() {
  let fuse = null;
  let searchIndex = null;
  let activeIndex = -1;

  const input = document.getElementById('search-input');
  const results = document.getElementById('search-results');

  if (!input || !results) return;

  // Load search index on first focus
  input.addEventListener('focus', loadIndex);

  // Handle input
  input.addEventListener('input', debounce(handleSearch, 150));

  // Handle keyboard navigation
  input.addEventListener('keydown', handleKeydown);

  // Close results when clicking outside
  document.addEventListener('click', function(e) {
    if (!e.target.closest('.search-container')) {
      closeResults();
    }
  });

  async function loadIndex() {
    if (searchIndex) return;

    try {
      const response = await fetch('/search-index.json');
      searchIndex = await response.json();

      fuse = new Fuse(searchIndex, {
        keys: [
          { name: 'title', weight: 3 },
          { name: 'headings.text', weight: 2 },
          { name: 'content', weight: 1 }
        ],
        includeMatches: true,
        threshold: 0.4,
        ignoreLocation: true,
        minMatchCharLength: 2
      });
    } catch (err) {
      console.error('Failed to load search index:', err);
    }
  }

  function handleSearch() {
    const query = input.value.trim();

    if (!query || !fuse) {
      closeResults();
      return;
    }

    const matches = fuse.search(query, { limit: 8 });

    if (matches.length === 0) {
      results.innerHTML = '<div class="search-no-results">No results found</div>';
      results.classList.add('open');
      activeIndex = -1;
      return;
    }

    results.innerHTML = matches.map((match, i) => {
      const item = match.item;
      const snippet = getSnippet(match, query);

      return `
        <a href="${item.path}" class="search-result" data-index="${i}">
          <div class="search-result-title">${escapeHtml(item.title)}</div>
          <div class="search-result-group">${escapeHtml(item.group)}</div>
          ${snippet ? `<div class="search-result-snippet">${snippet}</div>` : ''}
        </a>
      `;
    }).join('');

    results.classList.add('open');
    activeIndex = -1;
  }

  function getSnippet(match, query) {
    // Find content match
    const contentMatch = match.matches?.find(m => m.key === 'content');
    if (!contentMatch) return null;

    const content = match.item.content;
    const indices = contentMatch.indices[0];
    if (!indices) return null;

    const start = Math.max(0, indices[0] - 30);
    const end = Math.min(content.length, indices[1] + 50);

    let snippet = content.slice(start, end);
    if (start > 0) snippet = '...' + snippet;
    if (end < content.length) snippet = snippet + '...';

    // Highlight match
    const queryLower = query.toLowerCase();
    const snippetLower = snippet.toLowerCase();
    const matchStart = snippetLower.indexOf(queryLower);

    if (matchStart >= 0) {
      const before = escapeHtml(snippet.slice(0, matchStart));
      const matched = escapeHtml(snippet.slice(matchStart, matchStart + query.length));
      const after = escapeHtml(snippet.slice(matchStart + query.length));
      return before + '<mark>' + matched + '</mark>' + after;
    }

    return escapeHtml(snippet);
  }

  function handleKeydown(e) {
    const items = results.querySelectorAll('.search-result');
    if (!items.length) return;

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      activeIndex = Math.min(activeIndex + 1, items.length - 1);
      updateActive(items);
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      activeIndex = Math.max(activeIndex - 1, 0);
      updateActive(items);
    } else if (e.key === 'Enter' && activeIndex >= 0) {
      e.preventDefault();
      items[activeIndex].click();
    } else if (e.key === 'Escape') {
      closeResults();
      input.blur();
    }
  }

  function updateActive(items) {
    items.forEach((item, i) => {
      item.classList.toggle('active', i === activeIndex);
    });

    if (activeIndex >= 0) {
      items[activeIndex].scrollIntoView({ block: 'nearest' });
    }
  }

  function closeResults() {
    results.classList.remove('open');
    results.innerHTML = '';
    activeIndex = -1;
  }

  function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  function debounce(fn, delay) {
    let timeout;
    return function(...args) {
      clearTimeout(timeout);
      timeout = setTimeout(() => fn.apply(this, args), delay);
    };
  }
})();
