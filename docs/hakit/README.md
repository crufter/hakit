[Back](/)

Hakit
=====

The Hakit core (to see other submodules, click back on top) contains a Document type and utility functions on it.  
This type is modelled after the workings of dynamically typed languages, because a., it is easy to understand b., works well in
the highly dynamic world of web developement. However, since we are in Haskell, we do not throw all type safety out of the windows, like in
untyped langauges.

Let's begin with the definition of the type DocVal:

```haskell
> import Hakit
> :i DocVal
data DocVal
  = DocInt Integer
  | DocFloat Double
  | DocString Data.Text.Internal.Text
  | DocBool Bool
  | DocMap Document
  | DocList DList
  | DocTyped DTyped
  | Nil
```

(Note: naming suggestions are welcome)

A Document is basically a Map Text DocVal. We can create a Document with the following notation: 
(It will be easier to place this in a file)

```haskell
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import Hakit
import Data.Text

-- Note that this is not a doc yet.
example :: [(Text, DocVal)]
example = [
        "firstName"     .-  "Joe",
        "lastName"      .-  "Heisenberg",
        "age"           .-  33,
        "lovesTofu"     .-  True,
        "lived"         .-  [
            "London"        .- "2 years",
            "Debrecen"      .- "15 years",
            "San Francisco" .- "5 years"
        ],
        "kids"          .-  [d "Katie", d "David",  d "Amy"],
        "randomThings"  .-  [d 42,      d True,     d 3.25]
    ]

-- But we can create a document out of it easily:
exampleDoc = dm example
```

While Hakit in most places accepts both Document, and it's cousin [(Text, DocVal)], if not, you can stick a 'dm'
in front of the expression to get a document.

```haskell
> :t dm
dm :: DocComp d => d -> Document
```

The typeclass DocComp denotes types compatible with the Document type. This includes [(Text, DocVal] and the Document type itself.

You might have noticed that we have to stick a 'd' in front of list elements. That simply converts

We can easily do this conversion with the help of the aformentioned 'd' function:

```haskell
> :t d
d :: DocValComp a => a -> DocVal
```

The typeclass DocValComp denotes types compatible with the DocVal type. There is a lot of them so if you are curious issue a ':i DocValComp' or browse the source.  

More coming soon.