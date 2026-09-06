document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll(".post-copy-link").forEach(function (link) {
    link.addEventListener("click", async function (event) {
      event.preventDefault();

      const relativeUrl = link.dataset.postUrl;

      if (!relativeUrl) {
        return;
      }

      const url = new URL(relativeUrl, window.location.origin).href;

      try {
        await navigator.clipboard.writeText(url);

        const originalText = link.textContent;
        link.textContent = "[Copied]";

        window.setTimeout(function () {
          link.textContent = originalText;
        }, 1200);
      } catch (_) {
        window.prompt("Copy this post link:", url);
      }
    });
  });
});
