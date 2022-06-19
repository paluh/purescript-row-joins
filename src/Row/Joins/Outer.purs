module Row.Joins.Outer where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Tuple (Tuple)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map)
import Type.Eval.Row (Union)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Eval.Tuple (Tuple')
import Type.Prelude (Proxy(..))

foreign import data Just :: forall k. k -> Maybe k
foreign import data Nothing :: forall k. Maybe k

foreign import data Left :: forall k1 k2. k1 -> Either k1 k2
foreign import data Right :: forall k1 k2. k2 -> Either k1 k2

foreign import data Lift1 :: forall k1 k2. (k1 -> k2) -> k1 -> TypeExpr k2

instance Eval (Lift1 f a) (f a)

type MapRow :: forall k1 k2. (k1 -> TypeExpr k2) -> Row k1 -> TypeExpr (Row k2)
type MapRow f = ToRow <<< Map f <<< FromRow

type AccumConsElem :: forall k1 k2. Symbol -> k1 -> RowList k1 -> Maybe (Tuple (Maybe k2) (RowList k1))
type AccumConsElem key value list = Just (Tuple' Nothing (RL.Cons key value list))

type AccumStoreElem :: forall k1 k2 k3. k1 -> k2 -> k3 -> Maybe (Tuple (Maybe (Tuple k1 k2)) k3)
type AccumStoreElem key value list = Just (Tuple' (Just (Tuple' key value)) list)

type AccumNonElem :: forall k1 k2. k1 -> Maybe (Tuple (Maybe k2) k1)
type AccumNonElem list = Just (Tuple' Nothing list)

instance
  ( Eval accum accum'
  , DoStep null1 null2 key value accum' accum''
  ) =>
  Eval (Step null1 null2 key value accum) accum''

class DoStep :: forall k1 k2. k1 -> k2 -> Symbol -> Either k1 k2 -> Accum k1 k2 -> Accum k1 k2 -> Constraint
class DoStep n1 n2 l v acc acc' | n1 n2 l v acc -> acc'

instance DoStep n1 n2 key value Nothing (AccumStoreElem key value RL.Nil)
else instance DoStep n1 n2 key value (AccumNonElem list) (AccumStoreElem key value list)

else instance DoStep n1 n2 key (Left lv) (AccumStoreElem key (Right rv) list) (AccumConsElem key (Tuple' lv rv) list)
else instance DoStep n1 n2 key (Right rv) (AccumStoreElem key (Left lv) list) (AccumConsElem key (Tuple' lv rv) list)

else instance DoStep n1 n2 key' v' (AccumStoreElem key (Right rv) tail) (AccumStoreElem key' v' (RL.Cons key (Tuple' n1 rv) tail))
else instance DoStep n1 n2 key' v' (AccumStoreElem key (Left lv) tail) (AccumStoreElem key' v' (RL.Cons key (Tuple' lv n2) tail))

foreign import data Finallize :: forall k1 k2. k1 -> k2 -> Accum k1 k2 -> TypeExpr (RowList (k1 /\ k2))

instance Eval (Finallize n1 n2 Nothing) RL.Nil
instance Eval (Finallize n1 n2 (Just (Tuple' Nothing list))) list
instance Eval (Finallize n1 n2 (Just (Tuple' (Just (Tuple' k (Left v))) list))) (RL.Cons k (Tuple' v n2) list)
instance Eval (Finallize n1 n2 (Just (Tuple' (Just (Tuple' k (Right v))) list))) (RL.Cons k (Tuple' n1 v) list)

type MarkedElem k1 k2 = Symbol /\ (Either k1 k2)

type Accum k1 k2 = Maybe (Maybe (MarkedElem k1 k2) /\ RowList (k1 /\ k2))

-- | The first parameters are types which should be used as NULL placeholders for a particular kind.
foreign import data Step :: forall k1 k2. k1 -> k2 -> Symbol -> Either k1 k2 -> TypeExpr (Accum k1 k2) -> TypeExpr (Accum k1 k2)

foreign import data OuterJoin :: forall k1 k2. k1 -> k2 -> Row k1 -> Row k2 -> TypeExpr (Row (k1 /\ k2))

data NULL

type OuterJoin' = OuterJoin NULL NULL

-- | 1. `Either` based tagging
-- | 2. Merging tagged rows
-- | 3. Sorting through `RowToList`
-- | 4. "Pair based folding" of sorted row
instance
  ( Eval (MapRow (Lift1 Left) r1) r1'
  , Eval (MapRow (Lift1 Right) r2) r2'
  , Eval (Union r1' r2') u
  , Eval ((ToRow <<< Finallize null1 null2 <<< FoldrWithIndex (Step null1 null2) (Lift Nothing) <<< FromRow) u) u'
  ) =>
  Eval (OuterJoin null1 null2 r1 r2) u'

outerJoin :: forall l o r. Eval (OuterJoin' l r) o => Proxy l -> Proxy r -> Proxy o
outerJoin _ _ = Proxy

type LeftRow a = (foo :: a, bar :: String)
type RightRow = (foo :: Boolean, baz :: String)

z :: forall a. Proxy a -> Proxy (baz :: Tuple' NULL String, bar :: Tuple' String NULL, foo :: Tuple' a Boolean)
z _ = outerJoin (Proxy :: Proxy (LeftRow a)) (Proxy :: Proxy RightRow)
