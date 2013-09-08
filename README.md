hakit
======

**Hakit** (pronounce: "hack it") is my ongoing attempt to say thanks to the Haskell community.  

### What is it?
It is a web toolkit written in Haskell.

### Why?
The key idea is to introduce the least possible new concepts to people coming from other languages.

It especially eases the transition for people coming from dynamic languages by providing
a central dynamic data structure and using that in most places (where it is sensible),
so they will spend less time freaking out about yet another type they don't grasp yet.

#### Also...
PHP & co has approx 6000 web frameworks/toolkits (per language), Haskell has only a handful, let's get up
to speed, shall we?

### Modules

#### [Hakit](docs/hakit)
A module containing an easy to use data type similar to that of dynamic languages.

#### [Hakit.Http](docs/http)
A very simple to use HTTP server/client.

#### [Hakit.Validation](docs/validation)
Validate data easily.

#### Hakit.Db
Typeclasses (currently only one, a simplistic one) which reprents db drivers.

#### Hakit.Db.Mongo
MongoDB implementation of a db driver found in Hakit.Db

#### Hakit.Routing
Routing made easy.

### Separate modules intended to work well with Hakit:

#### [Haquery](https://github.com/recurziv/haquery)
A CSS selector based template engine (effectively, jQuery in Haskell, on your server).  
All selectors can be arbitrarily nested (eg: ".a:not(.b:has(.c, .d:first)):even" and similar).  
The workings of the selectors aim to match the jQuery behaviour, so for further documentation, consult the jQuery API.

#### [Jobs](https://github.com/recurziv/jobs)
Lets you start cronlike jobs from your Haskell application.

### Goals
- Make it so simple and obvious that one can pick it up in a couple of minutes.
- Be gentle to people with a non-fp background.
- Be fun to use.

**Progress**: very-very-very early stage.

**Disclaimer**: I am a Haskell newbie, thus the code is probably not that idiomatic. This will change though as I gain experience.