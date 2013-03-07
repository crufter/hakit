[Back](/)

Hakit.Validation
=====

### Basics

Builtin types:

    Type            Properties      Comment
            
	int             min, max        
	float           min, max
	string          min, max
	bool            
	const   

User defined validators are currently not implemented, but they will be soon.  

By default, if a field does not validate, the whole validation fails, unless the property "ignore" is
set, which means "ignore in case of failure".

Similarly, all validators can be used at validating "lists", with the list related properties:

    listMin         minimum list size
    listMax         maximum list size
    listIgnore      ignore list elements when they fail,
                    or consider the whole list failed

Currently, both the rules and subject must be of type Hakit.Document.

### Validating single fields.

In GHCi, first import the two needed modules, and set the OverloadedStrings and ExtendedDefaultRules pragmas:

```haskell
import Hakit
import Hakit.Validation
:set -XOverloadedStrings
:set -XExtendedDefaultRules
```

You can create documents with the function 'dm' (read: document), which converts Hakit.Document
compatible representations to a Hakit.Document. I formatted the following lines for
better readabiliy, obviously in GHCi you can't do it this way.

```haskell
let rules   = dm [
        "firstName" .- [
            "type"  .- "string",
            "min"   .- 500
        ],
        "lastName"  .- True
    ]
let subject = dm [
        "firstName"     .- "Joe",
        "lastName"      .- "Heisenberg",
        "randomField"   .- "Hello there."
    ]
```

True is a shorthand for ["type" .- "string"], which means, "it is a string, with an arbitrary length.  
If you don't specify a type, it will also default to string.

Let's try to validate the subject with the given rules.

```haskell
validateSafe rules subject
```

The validation failed with the next message:

```haskell
Left "string firstName is too short."
```

If we make the firstName property ignoreable:

```haskell
let rules   = dm [
        "firstName" .- [
            "type"      .- "string",
            "min"       .- 500,
            "ignore"    .- True,       -- Ignores the field if it does not validate
        ],
        "lastName" .- True
    ]
let subject = dm [
        "firstName"     .- "Joe",
        "lastName"      .- "Heisenberg",
        "randomField"   .- "Hello there."
    ]
```

By issuing "validateSafe rules subject" we get:

```
Right (fromList [("lastName",DocString "Heisenberg")])
```

RandomField was left out because it was not in the rules.  
FirstName was ignored because it didn't validate and it was specified as an ignoreable field.

### Validating list fields.

Every type can be validated as lists.

```haskell
let rules = dm [
        "tags" .- [
                                        -- We did not specify type, so it is a string.
            "min"           .- 2,       -- A tag must be at least 2 characters long.
            "max"           .- 50,      -- A tag must be at most 50 characters long.
            "ignore"        .- True,    -- If the tags field fails to validate,
                                        -- validation should proceed without it.
            "isList"        .- True,    -- We want a list of tags.
            "listIgnore"    .- True,    -- Elements of list which does not validate
                                        -- can be left out, so they won't cause the whole
                                        -- list to fail in validation.
            "listMin"       .- 3,       -- List must contain at least 3 elemets to declare it valid.
            "listMax"       .- 10       -- List can't be longer than 10 elements.
        ]
    ]
```