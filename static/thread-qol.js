document.addEventListener("DOMContentLoaded", function () {
  const button = document.createElement("a");

  button.id = "back-to-top";
  button.href = "#top";
  button.textContent = "↑ Top";
  button.title = "Back to top";

  document.body.appendChild(button);

  function updateButton() {
    button.style.display = window.scrollY > 500 ? "block" : "none";
  }

  window.addEventListener("scroll", updateButton, { passive: true });
  updateButton();
});
