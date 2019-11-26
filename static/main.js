hljs.initHighlightingOnLoad();

window.onload = function () {
    MathJax.Hub.Config({
        extensions: ["tex2jax.js"],
        jax: ["input/TeX", "output/HTML-CSS"],
        tex2jax: {
            inlineMath: [['$', '$'], ["\\(", "\\)"]],
            displayMath: [['$$', '$$'], ["\\[", "\\]"]],
            processEscapes: true
        },
        "HTML-CSS": { availableFonts: ["TeX"] }
    });
    $.get("/posts", function (data) {
        if (data === null || data.length === 0) {
            document.getElementById("loading-text").innerText = "No post found";
            return;
        } else {
            document.getElementById("loading-text").remove();
        }
        const base = document.getElementById("posts-billboard");
        base.appendChild(document.createElement("hr"));
        ggggg = data;
        for (post of data) {
            let component = document.createElement("div");
            let header = document.createElement("div");
            header.className = "header-info";
            header.innerHTML = "Created by : " + post.poster + ",   Date : " + new Date(post.date * 1000);
            component.appendChild(header);
            component.innerHTML += post.data;
            processHtmlFront(component);
            base.appendChild(component);
            base.appendChild(document.createElement("hr"));
        }
    })
}

function processHtmlFront(component) {
    codes = $('pre code', component);
    for (code of codes) {
        const lang = code.attributes.class.value;
        code.className = lang.replace("language-", "");
        hljs.highlightBlock(code);
    }
    MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
}
