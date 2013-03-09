[Back](/)

Hakit.Jobs
=====

With the help of Hakit.Jobs, you can start, manage and stop asynchronously, periodically running tasks.  

It is threadsafe and easy to use.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hakit.Jobs
```

Let's acquire a new job scheduler.

```haskell
> :t new
new :: IO Scheduler
> jobs <- new
```

Now this, by default is not running. Let's add two jobs to it.

```haskell
--              job name    interval    IO computation
> register jobs "1beep"     "7s"        $ putStr "\a"
> register jobs "2beep"     "11s"       $ putStr "\a\a"
```

If you try putStr "\a" by itself you can note that instead of printing out some weird character,  
it makes your computer beep. This is perfect for our use case because we can hear what's going on  
in an other thread.

We must start the execution of the jobs with the next command, but before that, we must change the clock speed  
of the scheduler, because by default it is aimed at slower tasks.

```haskell
> setInterval   jobs 1
> startExec     jobs
```

We now heard 3 beeps, because at start the scheduler runs all tasks. From that point, the scheduler will run the task according 
to the timing we specified:

    Example:        Description
    3s              Every 3 seconds
    5m              Every 5 minutes
    2h              Every 2 hours
    7d              Every 7 days
    2w              Every two weeks

We can stop the execution of task by issuing

```haskell
> stopExec jobs
```

More coming soon.
    

