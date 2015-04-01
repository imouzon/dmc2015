The 2015 Data Mining Cup
========================
* * * * * * * * * * * *

This is going to be an adventure!

#Welcome to our github page

##What do I do now

Congratulations - you have taken the first step in this competition.
Unless something goes wrong, we are going to be using Github as our main source of activity.
git is a powerful tool for collaborating - people can get your changes instantly and multiple people can even work on the same file.
If changes are made accidentally (or poorly) they can be reverted back to what they were.
While it may take some adjusting, using git is easy to pick up and you can get comfortable with it pretty quickly.

Since you can see this page it means that you have been added as a collaborater to the ISU DMC 2015 github repo.
All the files in the box above this one are now available to you. All you have to do is get git.

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

Exercise: Getting to Know the Team
==================================
Once you have pulled the repository, you will notice that there is a folder `team_pics`. 
If you look inside it you will see a picture labeled `ian.jpg`.
You may also notice a file on the repository page called `MeetTheTeam.md`.
If you click on it, you will see Ian's picture and a list with three items, 
two of which are true and one of which is false.

To orient ourselves with github and help us get to know one another, you will make one too.

##1. Add a picture to the `team_pics` folder.

##2. Edit the file `MeetTheTeam.md`. 
There is a template at the top of the file.
Copy it and replace the filler values with the link to your picture and your "two facts and a falsehood".

##3. Push the changes to the github folder
