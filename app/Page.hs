{-# LANGUAGE OverloadedStrings #-}
module Page where

import Lucid
import Data.String

staticMain = renderText (
    html_ $ do
        head_ $ do 
            title_ "Le Blog";
            termWith "script" [src_ "https://cdn.bootcss.com/jquery/3.4.1/jquery.min.js"] "";
            link_ [rel_ "stylesheet", href_ "https://cdn.bootcss.com/highlight.js/9.15.10/styles/default.min.css"];
            link_ [rel_ "stylesheet", href_ "./style.css"];
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/highlight.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/languages/rust.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/mathjax/2.7.6/MathJax.js?config=TeX-AMS-MML_HTMLorMML"] "";
            termWith "script" [src_ "./main.js"] "";
        body_ [style_ "background: url(https://cdn.jsdelivr.net/gh/frontendsophie/hexo-theme-autumn@1.0.0/source/img/button-bg.png) #f3f3f3"] $ do
            div_ [class_ "container"] $ do
                header_ [class_ "header"] $ do
                    h1_ [class_ "title"] $ do
                        a_ [href_ "/", class_ "logo"] "Welcome to the blogging system";
                    nav_ [class_ "links"] $ do
                        ul_ [class_ "hide-links"] $ do
                            li_ [] $ a_ [href_ "./post"] "Create new post";
                            li_ [] $ a_ [href_ "./register"] "Register";
                main_ [class_ "main"] $ do
                    h3_ "Recent Posts : ";
                    section_ [id_ "posts-billboard", class_ "posts", style_ "position: relative;"] $ do
                        div_ [class_ "flipping-load", id_ "loading-text"] "Now loading...";
                        )

dynamicUser username = renderText (
    html_ $ do
        head_ $ do 
            title_ $ fromString $ username ++ "'s Blog";
            termWith "script" [src_ "https://cdn.bootcss.com/jquery/3.4.1/jquery.min.js"] "";
            link_ [rel_ "stylesheet", href_ "https://cdn.bootcss.com/highlight.js/9.15.10/styles/default.min.css"];
            link_ [rel_ "stylesheet", href_ "/style.css"];
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/highlight.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/languages/rust.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/mathjax/2.7.6/MathJax.js?config=TeX-AMS-MML_HTMLorMML"] "";
            termWith "script" [src_ "/user.js"] "";
            meta_ [name_ "current-user", content_ $ fromString $ username];
        body_ [style_ "background: url(https://cdn.jsdelivr.net/gh/frontendsophie/hexo-theme-autumn@1.0.0/source/img/button-bg.png) #f3f3f3"] $ do
            div_ [class_ "container"] $ do
                header_ [class_ "header"] $ do
                    h1_ [class_ "title"] $ do
                        a_ [href_ "/", class_ "logo"] $ fromString $ "Welcome to " ++ username ++ "'s page";
                    nav_ [class_ "links"] $ do
                        ul_ [class_ "hide-links"] $ do
                            li_ [] $ a_ [href_ "/"] "Return";
                            li_ [] $ a_ [href_ "/register"] "Register";
                main_ [class_ "main"] $ do
                    h3_ "Recent Posts : ";
                    section_ [id_ "posts-billboard", class_ "posts", style_ "position: relative;"] $ do
                        div_ [class_ "flipping-load", id_ "loading-text"] "Now loading...";
                        )

dynamicPost pid username = renderText (
    html_ $ do
        head_ $ do 
            title_ $ fromString $ username ++ "'s Blog";
            termWith "script" [src_ "https://cdn.bootcss.com/jquery/3.4.1/jquery.min.js"] "";
            link_ [rel_ "stylesheet", href_ "https://cdn.bootcss.com/highlight.js/9.15.10/styles/default.min.css"];
            link_ [rel_ "stylesheet", href_ "/style.css"];
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/highlight.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/languages/rust.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/mathjax/2.7.6/MathJax.js?config=TeX-AMS-MML_HTMLorMML"] "";
            termWith "script" [src_ "/post.js"] "";
            meta_ [name_ "current-post", content_ $ fromString $ show pid];
        body_ [style_ "background: url(https://cdn.jsdelivr.net/gh/frontendsophie/hexo-theme-autumn@1.0.0/source/img/button-bg.png) #f3f3f3"] $ do
            div_ [class_ "container"] $ do
                header_ [class_ "header"] $ do
                    h1_ [class_ "title"] $ do
                        a_ [href_ "/", class_ "logo"] $ fromString $ username ++ "'s Blog";
                    nav_ [class_ "links"] $ do
                        ul_ [class_ "hide-links"] $ do
                            li_ [] $ a_ [href_ $ fromString $ "/u/" ++ username ] "Return";
                            li_ [] $ a_ [href_ "/register"] "Register";
                main_ [class_ "main"] $ do
                    article_ [id_ "posts-billboard", class_ "post"] $ do
                        div_ [class_ "flipping-load", id_ "loading-text"] "Now loading...";
                        )

staticPost = renderText (
    html_ $ do
        head_ $ title_ "Le Blog";
        body_ $ do
            h1_ "Create new post";
            hr_ [];
            a_ [href_ "./"] "Return";
            form_ [method_ "post"] $ do
                div_ "Title :";
                input_ [type_ "text", name_ "title"];
                br_ [];
                div_ "Context :";
                textarea_ [rows_ "6", cols_ "36", name_ "content"] "";
                br_ [];
                div_ "Secret code :";
                input_ [type_ "text", name_ "secret"];
                br_ [];
                input_ [type_ "submit"];
                        )