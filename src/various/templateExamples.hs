import qualified Text.StringTemplate as ST

type T = ST.StringTemplate String

t1 :: T
t1 = ST.newSTMP "Hello $name$."

simpleVar = ST.render $ ST.setAttribute "name" "Joey" t1

t2 :: T
t2 = ST.newSTMP "Looping: $names:{name|-$name $}$"

simpleLoop = ST.render $ ST.setAttribute "names" ["Joey", "Katie", "Amanda"] t2