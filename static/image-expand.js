document.addEventListener("DOMContentLoaded", function () {
  const imageLinks = document.querySelectorAll(".post-image-link");
  const expandLinks = document.querySelectorAll(".post-expand-link");

  if (!imageLinks.length && !expandLinks.length) {
    return;
  }

  const overlay = document.createElement("div");
  overlay.id = "image-overlay";
  overlay.setAttribute("aria-hidden", "true");

  const image = document.createElement("img");
  image.id = "image-overlay-image";

  overlay.appendChild(image);
  document.body.appendChild(overlay);

  function openImage(url, alt) {
    if (!url) {
      return;
    }

    image.src = url;
    image.alt = alt || "";

    overlay.classList.add("visible");
    overlay.setAttribute("aria-hidden", "false");
    document.body.style.overflow = "hidden";
  }

  function closeImage() {
    overlay.classList.remove("visible");
    overlay.setAttribute("aria-hidden", "true");
    image.removeAttribute("src");
    document.body.style.overflow = "";
  }

  imageLinks.forEach(function (link) {
    link.addEventListener("click", function (event) {
      event.preventDefault();

      const thumbnail = link.querySelector("img");

      openImage(
        link.href,
        thumbnail ? thumbnail.alt : ""
      );
    });
  });

  expandLinks.forEach(function (link) {
    link.addEventListener("click", function (event) {
      event.preventDefault();

      const post = link.closest(".post-container");
      const thumbnail = post
        ? post.querySelector(".post-image-link img")
        : null;

      openImage(
        link.dataset.imageUrl,
        thumbnail ? thumbnail.alt : ""
      );
    });
  });

  overlay.addEventListener("click", function (event) {
    if (event.target === overlay || event.target === image) {
      closeImage();
    }
  });

  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") {
      closeImage();
    }
  });
});
