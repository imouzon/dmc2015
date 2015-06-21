---
title       : Our 2015 Data Mining Cup Solution
subtitle    : Or How I Learned to Stop Worrying and Love the Grind
author      : ISU DMC 2015 Team 1
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]     # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---


--- .class #id 
## Slide 2


---
# Our 2015 Data Mining Cup Solution
## Iowa State University Team 1
### or
### How I Learned to Stop Worrying and Love the Grind
.footnote[A link to our [github page](https://github.com/imouzon/dmc2015)]
---
#Iowa 
#Iowa State University
##and 
#Our Team
---
## Iowa
### Where is Iowa

Because we know that this is an international audience, you might not all be familiar with the state of Iowa.
<iframe width="600" 
   height="450" 
   frameborder="0" 
   style="border:0"
   src="https://www.google.com/maps/embed/v1/directions?origin=Berlin%2C%20Germany&destination=Iowa%20State%20University%2C%20Ames%2C%20IA%2C%20United%20States&key=...">
</iframe>
---
## Iowa
### Where is Iowa
### About Iowa

## Iowa is in America's Midwest

<center>
   <img src="figs/iowa-skyline.jpg" alt="dmc logo" height="185"> 
</center>

-  The largest city, Des Moines, only has a population of about 600,000
-  The largest producer of corn, ethanol, and soybeans in the United States
-  There are more pigs than people in Iowa
-  It is named after the Iowa river which itself is named after the Native American "Ioway" tribe.
-  It is the birth place of the actor John Wayne (The Searchers, Red River, ...)
---
## Iowa
### Where is Iowa
### About Iowa
## Iowa State
### The Stats Dept

<center>
<img src="./figs/american_football.jpg" alt="dmc logo" height="145"> 
</center>

-  First statistics course offered in 1915
-  Department established in 1947 (the first statistics department in the US)
-  Snedecor Hall built in 1953
---
## Iowa
### Where is Iowa
### About Iowa
## Iowa State
### The Stats Dept
## Our Team
-  Great Teammates
<!--Pattern HTML-->
<ul class="g">
<li><a><img ~/dmc2015/team_pics/ian.jpeg alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Meiling.jpg alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Neo_Zhou.jpg alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Ran.jpg alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Sam_Benidt.jpg alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Weicheng.JPG alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Yaxun Sun.jpg alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Yet.JPG alt="Team Captain" /></a></li>
<li><a><img ~/dmc2015/team_pics/Yihua.JPG alt="Team Captain" /></a></li>
</div>
<ul>
<li><a href="#"><img ~/dmc2015/team_pics/alex.jpg alt="Team Captain" /></a></li>
<li><a href="#"><img ~/dmc2015/team_pics/chen.jpeg alt="Team Captain" /></a></li>
<li><a href="#"><img ~/dmc2015/team_pics/ian.jpeg alt="Team Captain" /></a></li>
<li><a href="#"><img ~/dmc2015/team_pics/penglh.jpg alt="Team Captain" /></a></li>
<li><a href="#"><img ~/dmc2015/team_pics/pete.jpg alt="Team Captain" /></a></li>
</ul>
---
#Our Approach
---
#In The First 10 Days
---
## First Steps
### Find Structure
### Examine Error

# Dealing with Loss 

$$
E = \\sum\_\{i=1\}^\{n\}\\Biggl\[ \\left\(\\frac\{u\_\{i\} - \\widehat\{u\}\_\{i\}\}\{\\frac\{1\}\{n\}\\sum\_\{i=1\}^\{n\}u\_\{i\}\}\\right\)^\{2\} + \\left\(\\frac\{v\_\{i\} - \\widehat\{v\}\_\{i\}\}\{\\frac\{1\}\{n\}\\sum\_\{i=1\}^\{n\}v\_\{i\}\}\\right\)^\{2\} + \\left\(\\frac\{w\_\{i\} - \\widehat\{w\}\_\{i\}\}\{\\frac\{1\}\{n\}\\sum\_\{i=1\}^\{n\}w\_\{i\}\}\\right\)^\{2\} + \\left\(\\frac\{b\_\{i\} - \\widehat\{b\}\_\{i\}\}\{\\frac\{1\}\{n\}\\sum\_\{i=1\}^\{n\}b\_\{i\}\}\\right\)^\{2\} \\Biggr\]
$$

Looking at this, we realized we needed to

-  Need to accurately predict coupon use
-  Need to accurately predict basket value
-  **and** accurately predict the weights
---
#  Picture of early rec ~ early_shop and usage 
---
#And Then...
---
#Nothing
We had nothing working for us at all
-  attempts to create some kind of base monetary value of coupons failed
-  attempts to fit a statistical model to user behavior failed
-  attempts to use "oddities" in the data failed
-  Too many categorical 
---
# Create new features
-  LLRs
-  tf-idf
---
# New features means new data divisions
-  Historical set 1 (desc. by Pete - mentioned by ian in background)
-  Historical set 3 (desc. by Ian - mentioned by Pete in background)
---
# Now features can be generated with ease
- LLRs on reward, user, ...
- LLRs on rewardXuser, ...
- LLRs on rewardXuserXbrand, ...
# Data Explosion! Thousands of features
---
# Reduce features by selection
- Pete - lasso and crf to reduce number of features
# Data Explosion! Thousands of features
- list of techinques for reduction
---
