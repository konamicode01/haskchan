document.addEventListener("DOMContentLoaded", function () {
  const postform = document.getElementById("postform");

  function isTyping(target) {
    if (!target) {
      return false;
    }

    const tag = target.tagName;

    return tag === "INPUT" ||
           tag === "TEXTAREA" ||
           tag === "SELECT" ||
           target.isContentEditable;
  }

  document.addEventListener("keydown", function (event) {
    const target = event.target;

    if (event.key === "Escape") {
      const imageOverlay = document.getElementById("image-overlay");

      if (imageOverlay && imageOverlay.classList.contains("visible")) {
        imageOverlay.classList.remove("visible");
        imageOverlay.setAttribute("aria-hidden", "true");

        const image = document.getElementById("image-overlay-image");
        if (image) {
          image.removeAttribute("src");
        }

        document.body.style.overflow = "";
        return;
      }

      if (window.location.hash === "#postform" && postform) {
        window.location.hash = "";
        return;
      }
    }

    if (isTyping(target)) {
      if (
        event.key === "Enter" &&
        event.ctrlKey &&
        postform &&
        postform.contains(target)
      ) {
        event.preventDefault();

        const submit = postform.querySelector('input[type="submit"]');

        if (submit) {
          submit.click();
        }
      }

      return;
    }

    if (event.key.toLowerCase() === "r" && postform) {
      event.preventDefault();

      window.location.hash = "postform";

      const textarea = postform.querySelector("textarea");

      if (textarea) {
        textarea.focus();
      }
    }
  });
});
