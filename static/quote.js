document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll("a.post-no").forEach(function (link) {
    link.addEventListener("click", function (event) {
      const match = link.textContent.match(/\d+/);

      if (!match) {
        return;
      }

      const postNo = match[0];
      const form = document.getElementById("postform");

      if (!form) {
        return;
      }

      const textarea = form.querySelector("textarea");

      if (!textarea) {
        return;
      }

      event.preventDefault();

      const quote = ">>" + postNo + "\n";
      const start = textarea.selectionStart || 0;
      const end = textarea.selectionEnd || 0;

      textarea.value =
        textarea.value.substring(0, start) +
        quote +
        textarea.value.substring(end);

      textarea.focus();

      const cursor = start + quote.length;
      textarea.setSelectionRange(cursor, cursor);

      // Preserve Haskchan's original #postform behavior.
      window.location.hash = "postform";

      // Make sure the form is visible and in view.
      form.scrollIntoView({
        behavior: "smooth",
        block: "start"
      });

      textarea.dispatchEvent(
        new Event("input", { bubbles: true })
      );
    });
  });
});
