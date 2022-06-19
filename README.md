# purescript-row-joins

Type level row joins.

## Usage

Outer join seems to be working:

```
module Test.README where

import Prelude
import Row.Joins.Outer (OuterJoin)
import Type.Eval (class Eval)
import Type.Prelude (Proxy(..))

outerJoin :: forall l o r. Eval (OuterJoin' l r) o => Proxy l -> Proxy r -> Proxy o
outerJoin _ _ = Proxy

type LeftRow a = (foo :: a, bar :: String)
type RightRow = (foo :: Boolean, baz :: String)

testOuterJoin :: forall a. Proxy a -> Proxy (baz :: Tuple' NULL String, bar :: Tuple' String NULL, foo :: Tuple' a Boolean)
testOuterJoin _ = outerJoin (Proxy :: Proxy (LeftRow a)) (Proxy :: Proxy RightRow)
```

The base `OuterJoin` allows you to pass different nulls types for left and right row so the join is really polymorphic. I mean we used `OuterJoin'` which is just specialized to `Type`:

```
data NULL

type OuterJoin' = OuterJoin NULL NULL
```

