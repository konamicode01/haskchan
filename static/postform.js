document.addEventListener("DOMContentLoaded", function () {
  const form = document.getElementById("postform");
  const header = document.getElementById("postform-header");

  if (!form || !header) {
    return;
  }

  const storageKey = "haskchan-postform-state";

  function clampPosition(left, top) {
    const maxLeft = Math.max(0, window.innerWidth - form.offsetWidth);
    const maxTop = Math.max(0, window.innerHeight - form.offsetHeight);

    return {
      left: Math.max(0, Math.min(left, maxLeft)),
      top: Math.max(0, Math.min(top, maxTop))
    };
  }

  function saveState() {
    const rect = form.getBoundingClientRect();

    localStorage.setItem(storageKey, JSON.stringify({
      left: rect.left,
      top: rect.top,
      width: form.offsetWidth,
      height: form.offsetHeight
    }));
  }

  function restoreState() {
    try {
      const saved = JSON.parse(localStorage.getItem(storageKey));

      if (!saved) {
        return;
      }

      const position = clampPosition(
        Number(saved.left) || 0,
        Number(saved.top) || 0
      );

      if (Number(saved.width) > 0) {
        form.style.width = saved.width + "px";
      }

      if (Number(saved.height) > 0) {
        form.style.height = saved.height + "px";
      }

      form.style.left = position.left + "px";
      form.style.top = position.top + "px";
      form.style.right = "auto";
    } catch (_) {
      localStorage.removeItem(storageKey);
    }
  }

  restoreState();

  let dragging = false;
  let offsetX = 0;
  let offsetY = 0;

  header.addEventListener("mousedown", function (event) {
    if (event.target.closest(".closebutton")) {
      return;
    }

    const rect = form.getBoundingClientRect();

    dragging = true;
    offsetX = event.clientX - rect.left;
    offsetY = event.clientY - rect.top;

    form.style.left = rect.left + "px";
    form.style.top = rect.top + "px";
    form.style.right = "auto";

    form.classList.add("dragging");

    event.preventDefault();
  });

  document.addEventListener("mousemove", function (event) {
    if (!dragging) {
      return;
    }

    const position = clampPosition(
      event.clientX - offsetX,
      event.clientY - offsetY
    );

    form.style.left = position.left + "px";
    form.style.top = position.top + "px";
  });

  document.addEventListener("mouseup", function () {
    if (!dragging) {
      return;
    }

    dragging = false;
    form.classList.remove("dragging");
    saveState();
  });

  if (window.ResizeObserver) {
    new ResizeObserver(function () {
      if (!dragging && getComputedStyle(form).display !== "none") {
        saveState();
      }
    }).observe(form);
  }

  window.addEventListener("resize", function () {
    const rect = form.getBoundingClientRect();

    const position = clampPosition(rect.left, rect.top);

    form.style.left = position.left + "px";
    form.style.top = position.top + "px";

    saveState();
  });
});
