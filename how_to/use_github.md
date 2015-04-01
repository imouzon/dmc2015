How To Use Github
=================

If you are new to github, you can think of github as a better version of dropbox that is more frustrating to work with.
That at least helped me when I was just starting to learn how to do things on here.

If you are very, very, very opposed to doing command line things then you are more than welcome to use
the github apps:

<center>
<h3>OSX: https://mac.github.com</h3>
<h3>Windows: https://windows.github.com</h3>
</center>

###OSX/Linux
There is no need to install anything on these systems. They both know the git command by default.
Since our repository is being hosted by the website Github, you may find it simpler to use the 
[Github application](https://mac.github.com/) to keep your work up-to-date with the team.
While I try to do most of my work from the command line, I still frequently use the app.
There is a great overview of how to do all the git things you usually 
need [here](http://rogerdudler.github.io/git-guide/).

#### Cloning the repo
This is how you get the files from the repo to your computer.
It is as simple as using the command line to navigate to the directory where you want to put the DMC folder and 
typing the following: 

`git clone https://github.com/imouzon/dmc2015`.

#### Updating the repo
Since when you start working it is likely that other people changed files, 
it is important to update the repo on your computer before you start working on 
new things. You can do this using 

`git pull`

#### Making changes and pushing them to the web. 
Because multiple team members may be working on the same file at the same time, 
git takes a little more to when you make changes to files or add new files, 
you first stage your changes using 

`git add *` 

then 

`git commit -m "<A helpful message descring your changes>"`.

Finally, the changes can be sent to the main repository using 

`git push`

Then you will be asked for your password.

###Windows
I recommend using the windows app https://windows.github.com/ provided by Github.
Like the mac app, there are some things you can do from the command line that you can't do in the app,
but you will most likely never need them.
