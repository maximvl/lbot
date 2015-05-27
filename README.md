# lbot
Jabber bot written in Common Lisp.

[![Build Status](https://travis-ci.org/maximvl/lbot.svg?branch=master)](https://travis-ci.org/maximvl/lbot)

# Setup
```
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
* (quicklisp-quickstart:install)
cd ~/quicklisp/local-projects/
git clone https://github.com/maximvl/lbot.git
# i recommend to start swank to be able to connect to node remotely
* (ql:quickload "swank")
* (swank:create-server :dont-close t)
* (ql:quickload "lbot")
* (lbot:connect "login" "password" :room "room@conference.server.org")
```
