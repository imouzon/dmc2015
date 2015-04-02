Connect to the DMC Server
=========================
![goodnews](pics/goodnews.png)

We have access to a cloud server for some of our bigger calculations.  All of you will have access to use this server for your computational needs -- you can email Me (Alex) or Ian and we should be able to give you an account on the server.  Theoretically, when we have to start running real computations we can scale up the server to handle larger datasets.  It is likely that this server will be faster and have more memory than your own personal computer.  

If you want to connect to the server you can connect over SSH.  The server IP address is: `104.236.207.198`.  This server is running Ubuntu Linux so you will need to understand a bit of the linux command line to run commands.

Linux/Mac OS
============
Connecting to a server over SSH is simple if you are comfortable using the terminal.  From the terminal you can connect using the following:

```
ssh USERNAME@104.236.207.198
```

Windows
=======
In order to connect to the server you will need an SSH client for windows.  If you use Chrome you can download a [Chrome extension](https://chrome.google.com/webstore/detail/secure-shell/pnhechapfaindjhompbnflcldabbghjo?hl=en) that will allow you to SSH into the server using the same command as in the Linux/Mac OS section.  If you do not use chrome you can download standalone SSH clients: [Putty](http://www.putty.org) or [WinSCP](http://www.winscp.net).  The instructions for how to use putty or winSCP should be on their respective websites.  Using the server IP address: `104.236.207.198` you should be able to connect.