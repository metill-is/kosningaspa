/* Kosningaspá methodology notes: typeset maths (KaTeX auto-render) and highlight code (highlight.js).
   Both libraries load from CDN with `defer`; the page reads fine if either is offline. */
window.addEventListener("DOMContentLoaded", function () {
  if (window.renderMathInElement) {
    renderMathInElement(document.body, {
      delimiters: [
        {left: "$$", right: "$$", display: true},
        {left: "$", right: "$", display: false},
        {left: "\\[", right: "\\]", display: true},
        {left: "\\(", right: "\\)", display: false}
      ],
      ignoredTags: ["script", "noscript", "style", "textarea", "pre", "code"],
      throwOnError: false
    });
  }
  if (window.hljs) {
    document.querySelectorAll("pre code").forEach(function (b) { window.hljs.highlightElement(b); });
  }
});
