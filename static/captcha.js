document.addEventListener("DOMContentLoaded", function () {
  const captcha = document.getElementById("captcha");

  if (!captcha) {
    return;
  }

  captcha.style.cursor = "pointer";
  captcha.title = "Click to refresh CAPTCHA";

  captcha.addEventListener("click", function () {
    captcha.src = "/.phi/captcha-refresh.jpg?refresh=" + Date.now();
  });
});
