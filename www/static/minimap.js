(function() {
  'use strict';

  function init() {
    const content = document.querySelector('.content');
    if (!content) return;

    // Get the inner div that holds the actual content
    const contentInner = content.querySelector(':scope > div');
    if (!contentInner) return;

    const headings = contentInner.querySelectorAll('h2[id], h3[id]');
    if (headings.length === 0) return;

    // Create minimap container
    const minimap = document.createElement('nav');
    minimap.className = 'minimap';
    minimap.setAttribute('aria-label', 'Page sections');

    // Add header
    const header = document.createElement('div');
    header.className = 'minimap-header';
    header.textContent = 'On this page';
    minimap.appendChild(header);

    // Build minimap items
    const items = [];
    headings.forEach(heading => {
      const item = document.createElement('a');
      item.href = '#' + heading.id;
      item.textContent = heading.textContent;
      item.className = 'minimap-item';
      if (heading.tagName === 'H3') {
        item.classList.add('minimap-item-sub');
      }
      item.dataset.targetId = heading.id;
      minimap.appendChild(item);
      items.push({ element: item, target: heading });
    });

    // Insert minimap inside content, after the inner div
    content.appendChild(minimap);

    // Click handler for smooth scroll
    minimap.addEventListener('click', function(e) {
      const link = e.target.closest('.minimap-item');
      if (!link) return;

      e.preventDefault();
      const targetId = link.dataset.targetId;
      const target = document.getElementById(targetId);
      if (target) {
        const targetRect = target.getBoundingClientRect();
        const contentRect = content.getBoundingClientRect();
        const scrollTop = content.scrollTop + targetRect.top - contentRect.top - 230;
        content.scrollTo({ top: scrollTop, behavior: 'smooth' });
        history.pushState(null, '', '#' + targetId);
      }
    });

    // Scroll tracking with IntersectionObserver
    let currentActive = null;

    const observer = new IntersectionObserver(
      function(entries) {
        // Find all currently intersecting headings
        const visible = [];
        items.forEach(item => {
          const rect = item.target.getBoundingClientRect();
          // Consider heading visible if it's in the top 60% of viewport
          if (rect.top >= 0 && rect.top < window.innerHeight * 0.6) {
            visible.push(item);
          }
        });

        // Also check headings that are above viewport (user scrolled past)
        if (visible.length === 0) {
          // Find the last heading that's above the viewport
          for (let i = items.length - 1; i >= 0; i--) {
            const rect = items[i].target.getBoundingClientRect();
            if (rect.top < window.innerHeight * 0.3) {
              visible.push(items[i]);
              break;
            }
          }
        }

        // Activate the topmost visible heading
        const toActivate = visible[0];

        if (toActivate && toActivate !== currentActive) {
          if (currentActive) {
            currentActive.element.classList.remove('minimap-item-active');
          }
          toActivate.element.classList.add('minimap-item-active');
          currentActive = toActivate;
        }
      },
      {
        root: content,
        rootMargin: '-10% 0px -60% 0px',
        threshold: 0
      }
    );

    // Observe all headings
    items.forEach(item => observer.observe(item.target));

    // Initial scroll position check
    function checkInitialPosition() {
      for (let i = items.length - 1; i >= 0; i--) {
        const rect = items[i].target.getBoundingClientRect();
        if (rect.top < window.innerHeight * 0.4) {
          if (currentActive) {
            currentActive.element.classList.remove('minimap-item-active');
          }
          items[i].element.classList.add('minimap-item-active');
          currentActive = items[i];
          break;
        }
      }
      // If nothing found, activate first item
      if (!currentActive && items.length > 0) {
        items[0].element.classList.add('minimap-item-active');
        currentActive = items[0];
      }
    }

    // Run initial check and also on scroll (backup for IntersectionObserver edge cases)
    checkInitialPosition();
    content.addEventListener('scroll', function() {
      requestAnimationFrame(checkInitialPosition);
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
