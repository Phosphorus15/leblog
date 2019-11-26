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
        for (post of data) {
            let component = document.createElement("article");
            let read = document.createElement("div");
            let articleTo = document.createElement("a");
            let readTo = document.createElement("a");
            articleTo.innerText = post.title;
            readTo.className = "more-link";
            readTo.href = "/p/" + post.id;
            articleTo.href = "/p/" + post.id;
            read.className = "post-content";
            readTo.innerText = "Read article";
            read.appendChild(readTo);
            let title = document.createElement("h1");
            title.className = "post-title";
            title.appendChild(articleTo);
            component.className = "post";
            let header = document.createElement("ul");
            let author = document.createElement("li");
            let nested = document.createElement("a");
            nested.href = "/u/" + post.poster;
            let date = document.createElement("li");
            header.className = "post-date";
            header.appendChild(author);
            header.appendChild(date);
            date.innerText = new Date(post.date * 1000).toLocaleDateString();
            nested.style = "text-decoration: none; color: #bbb";
            nested.innerText = post.poster;
            author.appendChild(nested);
            component.appendChild(title);
            component.appendChild(header);
            component.appendChild(read);
            processHtmlFront(component);
            base.appendChild(component);
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
