Write a Presentation in Markdown
================================

Markdown is a very simple syntax to learn.
It has many incredible uses.
For instance, not only is it very readable as raw text,
but many websites make use of tools to recognize markdown syntax
and display them like HTML. In fact, alternating between markdown and HTML
makes writing webpages very easy.

I recently learned to write presentations in markdown using [remark](https://github.com/gnab/remark).
It took some tweaking to make a presentation look as good as his [illustration](http://remarkjs.com/#1),
but I finally got something going:
### You're going to need CSS
There is an HTML wrapper around the markdown portion of the slides:
```HTML
<!DOCTYPE html>
<html>
<head>
   <title>Title</title>
   <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
   <style type="text/css">
      @import url(https://fonts.googleapis.com/css?family=Yanone+Kaffeesatz);
      @import url(https://fonts.googleapis.com/css?family=Droid+Serif:400,700,400italic);
      @import url(https://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,400italic);

   body { font-family: 'Droid Serif'; }
   h1, h2, h3 {
      font-family: 'Yanone Kaffeesatz';
      font-weight: normal;
   }
   .remark-code, .remark-inline-code { font-family: 'Ubuntu Mono'; }
   </style>
</head>

<body>
   <textarea id="source">

   <!-- Add slides here -->

   </textarea>
   <script src="https://gnab.github.io/remark/downloads/remark-latest.min.js">
   </script>
   <script>
      var slideshow = remark.create();
   </script>
</body>
</html>
```
Breaking this down piece by piece we can see that:
Looking at each piece we see the following pieces:
```HTML
<!DOCTYPE html>
<html>
<head>
   <title>Title</title>
   <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
   <!-- Leaving Stuff Out
      .
      .  
      .
        Leaving Stuff Out-->
</head>

<body>
   <textarea id="source">

   <!-- Add slides here -->

   </textarea>
   <!-- Leaving Stuff Out
      .
      .  
      .
        Leaving Stuff Out-->
</body>
</html>
```
Is the standard HTML declaration stuff 
(a [`DOCTYPE` declaration](http://www.w3schools.com/tags/tag_DOCTYPE.asp),
and the [head and body elements](https://andrewhlee.wordpress.com/2013/02/22/what-goes-inside-the-head-and-body-of-the-code/).
We would probably like to change `Title` to the actual title of our presentation.

Next, we have fonts to import from [google developers](https://developers.google.com/fonts/docs/getting_started):
The fonts can be previewed at [www.google.com/fonts](http://www.google.com/fonts).
This section gets the fonts from google 
(`@import url()` is apparently [not the preferred way of doing this](http://stackoverflow.com/questions/10036977/best-way-to-include-css-why-use-import)) 
and specifies that 
we would like to use `Droid Serif` for the body text,
`Yanone Kaffeesatz` for the headers, and `Ubuntu Mono` for code:
```HTML
   <style type="text/css">
      @import url(https://fonts.googleapis.com/css?family=Yanone+Kaffeesatz);
      @import url(https://fonts.googleapis.com/css?family=Droid+Serif:400,700,400italic);
      @import url(https://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,400italic);

   body { font-family: 'Droid Serif'; }
   h1, h2, h3 {
      font-family: 'Yanone Kaffeesatz';
      font-weight: normal;
   }
   .remark-code, .remark-inline-code { font-family: 'Ubuntu Mono'; }
   </style>
```

The last piece that is interesting is
```HTML
   <script src="https://gnab.github.io/remark/downloads/remark-latest.min.js">
   </script>
```
which loads loads the JavaScript file at the specified url 
[after the page has loaded](http://javascript.info/tutorial/adding-script-html#scripts-at-the-end-of-body).
You can visit this page yourself, where you will see an enormous amount of
code - this must be the code that takes the markdown you used to write the file 
and converts it to HTML the browser is able to display.
But when we write this code, we get something less like the example and 
more like [a boring wall of text](../presentation/templates/boring.pdf).

This is because of CSS. If we look at the beautiful demonstration template we see the problem right away: 
```HTML
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <meta name="keywords" content="remark,remarkjs,markdown,slideshow,presentation" />
    <meta name="description" content="A simple, in-browser, markdown-driven slideshow tool." />
    <title>Remark</title>
    <style type="text/css">
      @import url(//fonts.googleapis.com/css?family=Droid+Serif);
      @import url(//fonts.googleapis.com/css?family=Yanone+Kaffeesatz);
      @import url(//fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,400italic);

      body {
        font-family: 'Droid Serif';
      }
      h1, h2, h3 {
        font-family: 'Yanone Kaffeesatz';
        font-weight: 400;
        margin-bottom: 0;
      }
      .remark-slide-content h1 { font-size: 3em; }
      .remark-slide-content h2 { font-size: 2em; }
      .remark-slide-content h3 { font-size: 1.6em; }
      .footnote {
        position: absolute;
        bottom: 3em;
      }
      li p { line-height: 1.25em; }
      .red { color: #fa0000; }
      .large { font-size: 2em; }
      a, a > code {
        color: rgb(249, 38, 114);
        text-decoration: none;
      }
      code {
        -moz-border-radius: 5px;
        -web-border-radius: 5px;
        background: #e7e8e2;
        border-radius: 5px;
      }
      .remark-code, .remark-inline-code { font-family: 'Ubuntu Mono'; }
      .remark-code-line-highlighted     { background-color: #373832; }
      .pull-left {
        float: left;
        width: 47%;
      }
      .pull-right {
        float: right;
        width: 47%;
      }
      .pull-right ~ p {
        clear: both;
      }
      #slideshow .slide .content code {
        font-size: 0.8em;
      }
      #slideshow .slide .content pre code {
        font-size: 0.9em;
        padding: 15px;
      }
      .inverse {
        background: #272822;
        color: #777872;
        text-shadow: 0 0 20px #333;
      }
      .inverse h1, .inverse h2 {
        color: #f3f3f3;
        line-height: 0.8em;
      }

      /* Slide-specific styling */
      #slide-inverse .footnote {
        bottom: 12px;
        left: 20px;
      }
      #slide-how .slides {
        font-size: 0.9em;
        position: absolute;
        top:  151px;
        right: 140px;
      }
      #slide-how .slides h3 {
        margin-top: 0.2em;
      }
      #slide-how .slides .first, #slide-how .slides .second {
        padding: 1px 20px;
        height: 90px;
        width: 120px;
        -moz-box-shadow: 0 0 10px #777;
        -webkit-box-shadow: 0 0 10px #777;
        box-shadow: 0 0 10px #777;
      }
      #slide-how .slides .first {
        background: #fff;
        position: absolute;
        top: 20%;
        left: 20%;
        z-index: 1;
      }
      #slide-how .slides .second {
        position: relative;
        background: #fff;
        z-index: 0;
      }

      /* Two-column layout */
      .left-column {
        color: #777;
        width: 20%;
        height: 92%;
        float: left;
      }
        .left-column h2:last-of-type, .left-column h3:last-child {
          color: #000;
        }
      .right-column {
        width: 75%;
        float: right;
        padding-top: 1em;
      }
    </style>
  </head>
  <body>
    <textarea id="source">
    <!--
    Leaving out the slides
    -->
    </textarea>
    <script src="downloads/remark-latest.min.js" type="text/javascript"></script>
    <script type="text/javascript">
      var hljs = remark.highlighter.engine;
    </script>
    <script src="remark.language.js" type="text/javascript"></script>
    <script type="text/javascript">
      var slideshow = remark.create({
          highlightStyle: 'monokai',
          highlightLanguage: 'remark'
        }) ;
    </script>
    <script type="text/javascript">
      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', 'UA-44561333-1']);
      _gaq.push(['_trackPageview']);

      (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
      })();
    </script>
  </body>
</html>
```
Take the piece:
```HTML
   .right-column {
     width: 75%;
     float: right;
     padding-top: 1em;
   }
```
This puts a column that takes of 75% of the slides width on the right side (`float: right;`) of the slide (or whatever container it finds itself in) 
and pads it on the top just a little (`padding-top: 1em;`) so that it won't touch the elements above it.
It's pretty self exlanatory, as are the roles of `.left-column`,
`.remark-slide-content h1`, `.remark-slide-content h2`, `.remark-slide-content h3 { font-size: 1.6em; }`, 
`.footnote`, `li`, `code`, `.remark-code`, `.remark-inline-code`, `.remark-code-line-highlighted`, 
`.pull-left`, `.pull-right`, `.inverse`, `.inverse h1`, and `.inverse h2`
if you take a second to look at them.
The `#slideshow` elements is standard [CSS slide show stuff](http://jonraasch.com/blog/a-simple-jquery-slideshow).
This leaves
```HTML
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <meta name="keywords" content="remark,remarkjs,markdown,slideshow,presentation" />
    <meta name="description" content="A simple, in-browser, markdown-driven slideshow tool." />
    <!--
       Leaving out the CSS stuff we can understand at a glance
    -->
  </head>
  <body>
    <textarea id="source">
    <!--
       Leaving out the slides
    -->
    </textarea>
    <script src="downloads/remark-latest.min.js" type="text/javascript"></script>
    <script type="text/javascript">
      var hljs = remark.highlighter.engine;
    </script>
    <script src="remark.language.js" type="text/javascript"></script>
    <script type="text/javascript">
      var slideshow = remark.create({
          highlightStyle: 'monokai',
          highlightLanguage: 'remark'
        }) ;
    </script>
    <script type="text/javascript">
      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', 'UA-44561333-1']);
      _gaq.push(['_trackPageview']);

      (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
      })();
    </script>
  </body>
</html>
```
a few JavaScript elements to be run at the end. If we take it on faith that we need all of these, then we keep this stuff,
and only edit the CSS stuff to make our slides look like we want them too.
We can keep the style sheets in their own file, `dmc_style1.css`, 
save the built in JavaScript elements (`varslideshow.js`, `gaqpush.js`) and we can reduce the template to:
```HTML
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <meta name="keywords" content="the presentation keywords" />
    <meta name="description" content="describe the presentation" />
    <link rel="stylesheet" type="text/css" href="../templates/dmc_style1.css">
  </head>
  <body>
    <textarea id="source">

    <!-- Add slides here -->

    </textarea>
    <script src="downloads/remark-latest.min.js" type="text/javascript"></script>
    <script type="text/javascript"> var hljs = remark.highlighter.engine; </script>
    <script src="remark.language.js" type="text/javascript"></script>
    <script src="../templates/js/varslideshow.js" type="text/javascript"></script>
    <script src="../templates/js/gaqpush.js" type="text/javascript"></script>
  </body>
</html>
```
which is actually very simple.
