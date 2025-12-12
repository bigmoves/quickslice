// Mobile navigation toggle
(() => {
  const toggleSidebar = () => {
    document.querySelector('.sidebar')?.classList.toggle('open');
    document.querySelector('.sidebar-backdrop')?.classList.toggle('open');
  };

  const closeSidebar = () => {
    document.querySelector('.sidebar')?.classList.remove('open');
    document.querySelector('.sidebar-backdrop')?.classList.remove('open');
  };

  const init = () => {
    document.querySelector('.menu-toggle')?.addEventListener('click', toggleSidebar);
    document.querySelector('.sidebar-backdrop')?.addEventListener('click', toggleSidebar);

    // Close sidebar when clicking nav links on mobile
    document.querySelectorAll('.sidebar a').forEach(link => {
      link.addEventListener('click', () => {
        if (window.innerWidth < 768) closeSidebar();
      });
    });
  };

  document.readyState === 'loading'
    ? document.addEventListener('DOMContentLoaded', init)
    : init();
})();
