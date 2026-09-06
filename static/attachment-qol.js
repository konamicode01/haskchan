document.addEventListener("DOMContentLoaded", function () {
  const toggles = document.querySelectorAll(".post-attachment-toggle");

  if (!toggles.length) {
    return;
  }

  toggles.forEach(function (toggle) {
    const post = toggle.closest(".post");
    const content = post
      ? post.querySelector(".post-attachment-content")
      : null;

    if (!post || !content) {
      return;
    }

    const key = "haskchan-attachment-" + post.id;

    function setCollapsed(collapsed, save) {
      content.classList.toggle("collapsed", collapsed);
      toggle.textContent = collapsed ? "[Show]" : "[Hide]";

      if (save) {
        localStorage.setItem(key, collapsed ? "1" : "0");
      }
    }

    const saved = localStorage.getItem(key);
    setCollapsed(saved === "1", false);

    toggle.addEventListener("click", function (event) {
      event.preventDefault();

      const collapsed = !content.classList.contains("collapsed");
      setCollapsed(collapsed, true);
    });
  });
});
