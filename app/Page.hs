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
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/highlight.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/languages/rust.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/mathjax/2.7.6/MathJax.js?config=TeX-AMS-MML_HTMLorMML"] "";
            termWith "script" [src_ "./main.js"] "";
        body_ $ do
            h1_ "Welcome to the blogging system";
            hr_ [];
            a_ [href_ "./post"] "Create new post";
            a_ [class_ "padding"] "   ";
            a_ [href_ "./register"] "Register";
            h3_ "Recent Posts : ";
            div_ [class_ "posts-billboard", id_ "posts-billboard"] $ do
                div_ [class_ "flipping-load", id_ "loading-text"] "Now loading...";
                        )

dynamicUser username = renderText (
    html_ $ do
        head_ $ do 
            title_ $ fromString $ username ++ "'s Blog";
            termWith "script" [src_ "https://cdn.bootcss.com/jquery/3.4.1/jquery.min.js"] "";
            link_ [rel_ "stylesheet", href_ "https://cdn.bootcss.com/highlight.js/9.15.10/styles/default.min.css"];
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/highlight.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/highlight.js/9.15.10/languages/rust.min.js"] "";
            termWith "script" [src_ "https://cdn.bootcss.com/mathjax/2.7.6/MathJax.js?config=TeX-AMS-MML_HTMLorMML"] "";
            termWith "script" [src_ $ "/user.js" ] "";
            meta_ [name_ "current-user", content_ $ fromString $ username];
        body_ $ do
            h1_ $ fromString $ "Welcome to " ++ username ++ "'s page";
            hr_ [];
            a_ [href_ "/"] "Return";
            a_ [class_ "padding"] "   ";
            a_ [href_ "/register"] "Register";
            h3_ "Posts : ";
            div_ [class_ "posts-billboard", id_ "posts-billboard"] $ do
                div_ [class_ "flipping-load", id_ "loading-text"] "Now loading...";
                        )

staticPost = renderText (
    html_ $ do
        head_ $ title_ "Phosphorus' Blog";
        body_ $ do
            h1_ "Create new post";
            hr_ [];
            a_ [href_ "./"] "Return";
            form_ [method_ "post"] $ do
                textarea_ [rows_ "6", cols_ "36", name_ "content"] "";
                br_ [];
                div_ "Secret code :";
                input_ [type_ "text", name_ "secret"];
                br_ [];
                input_ [type_ "submit"];
                        )