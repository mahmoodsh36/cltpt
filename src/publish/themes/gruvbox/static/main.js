document.addEventListener('DOMContentLoaded', () => {
  const themeToggle = document.getElementById('themeToggle');
  const body = document.body;
  const icon = themeToggle?.querySelector('i');

  const updateIcon = (isDark) => {
    if (!icon) return;
    if (isDark) {
      icon.classList.remove('fa-sun');
      icon.classList.add('fa-moon');
    } else {
      icon.classList.remove('fa-moon');
      icon.classList.add('fa-sun');
    }
  };

  // load saved theme or use default (dark-theme)
  const savedTheme = localStorage.getItem('theme') || 'dark-theme';
  if (savedTheme === 'light-theme') {
    body.classList.remove('dark-theme');
    body.classList.add('light-theme');
    updateIcon(false);
  } else {
    body.classList.add('dark-theme');
    body.classList.remove('light-theme');
    updateIcon(true);
  }

  if (themeToggle) {
    themeToggle.addEventListener('click', () => {
      const isDarkMode = body.classList.contains('dark-theme');
      if (isDarkMode) {
        body.classList.remove('dark-theme');
        body.classList.add('light-theme');
        localStorage.setItem('theme', 'light-theme');
        updateIcon(false);
      } else {
        body.classList.remove('light-theme');
        body.classList.add('dark-theme');
        localStorage.setItem('theme', 'dark-theme');
        updateIcon(true);
      }
    });
  }
});