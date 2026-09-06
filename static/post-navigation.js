document.addEventListener("DOMContentLoaded", function () {
  function highlightTarget() {
    document.querySelectorAll(".post.target-highlight").forEach(function (post) {
      post.classList.remove("target-highlight");
    });

    const hash = window.location.hash;

    if (!hash || !hash.startsWith("#post")) {
      return;
    }

    const post = document.getElementById(hash.substring(1));

    if (!post) {
      return;
    }

    post.classList.remove("target-highlight");
    void post.offsetWidth;
    post.classList.add("target-highlight");
  }

  highlightTarget();
  window.addEventListener("hashchange", highlightTarget);
});
