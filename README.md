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

#### Hakit.Server
A very simple to use HTTP server.

#### [Hakit.Validation](docs/validation)
Validate data easily.

#### [Hakit.Spice](docs/spice)
A CSS selector based template engine (effectively, jQuery in Haskell, on your server).  
All selectors can be arbitrarily nested (eg: ".a:not(.b:has(.c, .d:first)):even" and similar).  
The workings of the selectors aim to match the jQuery behaviour, so for further documentation, consult the jQuery API.

Selectors planned:

    Implemented     Example                     Name
    Y               *                           - All selector
    Y               #id                         - Id selector
    Y               .class                      - Class selector
    Y               selector1 selector2         - Descendant selector
    Y               type                        - Type selector
    Y               selector1 > selector2       - Direct child selector
    Y               [attrName]                  - Has attribute selector
    Y               [attrName="val"]            - Attribute equals selector
    Y               [attrName*="val"]           - Attribute contains selector
    Y               [attrName^="val"]           - Attribute starts with selector
    Y               [attrName$="val"]           - Attribute ends with selector
    Y               [attrName~="val"]           - Attribute contains word selector
    Y               [attrName!="val"]           - Attribute not equals selector
    Y               selector1, selector2        - Multiple selectors selector
    Y               prev + next                 - Next adjacent selector
    Y               prev ~ siblings             - Next siblings selector
    Y               :not(selector)              - :not() selector
    Y               :has(selector)              - :has() selector
    Y               :eq(3)                      - :eq() selector
    Y               :lt(3)                      - :lt() selector
    Y               :gt(3)                      - :gt() selector
    Y               :even                       - :even selector
    Y               :odd                        - :odd selector
    Y               :first                      - :first selector
    Y               :last                       - :last selector
    Y               :first-child                - :first-child selector
    Y               :last-child                 - :last-child selector
    Y               :nth-child(3)               - :nth-child() selector
    Y               :nth-last-child(3)          - :nth-last-child() selector
    Y               :empty                      - :empty selector
    Y               :parent                     - :parent selector

See the tests of Spice for usage examples.

#### Hakit.Db
Typeclasses (currently only one, a simplistic one) which reprents db drivers.

#### Hakit.Db.Mongo
MongoDB implementation of a db driver found in Hakit.Db

#### Hakit.Routing
Routing made easy.

#### [Hakit.Jobs](docs/jobs)
Lets you start cronlike jobs from your Haskell application.

### Goals
- Make it so simple and obvious that one can pick it up in a couple of minutes.
- Be gentle to people with a non-fp background.
- Be fun to use.
- Reduce time to market of unique web apps by providing tremendous amount of flexibility.

**Progress**: very-very-very early stage.

**Disclaimer**: I am a Haskell newbie, thus the code is probably not that idiomatic. This will change though as I gain experience.