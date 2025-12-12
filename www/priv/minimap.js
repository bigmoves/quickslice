(function() {
  'use strict';

  function init() {
    const content = document.querySelector('.content');
    const minimap = document.querySelector('.minimap');
    if (!content || !minimap) return;

    // Get the inner div that holds the actual content
    const contentInner = content.querySelector(':scope > div');
    if (!contentInner) return;

    // Build items array from server-rendered minimap links
    const items = [];
    minimap.querySelectorAll('.minimap-item').forEach(link => {
      const targetId = link.dataset.targetId;
      const target = document.getElementById(targetId);
      if (target) {
        items.push({ element: link, target: target });
      }
    });

    if (items.length === 0) return;

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

    // Scroll tracking
    let currentActive = null;

    function checkScrollPosition() {
      for (let i = items.length - 1; i >= 0; i--) {
        const rect = items[i].target.getBoundingClientRect();
        if (rect.top < window.innerHeight * 0.4) {
          if (currentActive !== items[i]) {
            if (currentActive) {
              currentActive.element.classList.remove('minimap-item-active');
            }
            items[i].element.classList.add('minimap-item-active');
            currentActive = items[i];
          }
          return;
        }
      }
      // If nothing found, activate first item
      if (!currentActive && items.length > 0) {
        items[0].element.classList.add('minimap-item-active');
        currentActive = items[0];
      }
    }

    // Run initial check and on scroll
    checkScrollPosition();
    content.addEventListener('scroll', function() {
      requestAnimationFrame(checkScrollPosition);
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
