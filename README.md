# purescript-row-joins

Type level row joins.

## Usage

Outer join seems to be working:

```purescript
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

```purescript
data NULL

type OuterJoin' = OuterJoin NULL NULL
```


## How?

```purescript
-- | 1. `Either` based tagging
-- | 2. Merging tagged rows
-- | 3. Sorting through `RowToList`
-- | 4. "Pair based folding" of sorted list
instance
  ( Eval (MapRow (Lift1 Left) r1) r1'
  , Eval (MapRow (Lift1 Right) r2) r2'
  , Eval (Union r1' r2') u
  , Eval ((ToRow <<< Finallize null1 null2 <<< FoldrWithIndex (Step null1 null2) (Lift Nothing) <<< FromRow) u) u'
  ) =>
  Eval (OuterJoin null1 null2 r1 r2) u'
```
