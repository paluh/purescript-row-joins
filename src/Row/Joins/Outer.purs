module Row.Joins.Outer where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Prim.Row as R
import Prim.RowList (RowList)
import Prim.RowList as RL
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<), App)
import Type.Eval.Functor (Map)
import Type.Eval.Row (Union)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Eval.Tuple (Snd, Tuple')
import Type.Prelude (Proxy(..))

foreign import data Just :: forall k. k -> Maybe k
foreign import data Nothing :: forall k. Maybe k

foreign import data FromMaybe :: forall k. k -> Maybe k -> TypeExpr k

instance Eval (FromMaybe a Nothing) a
instance Eval (FromMaybe a (Just a')) a'

foreign import data Left :: forall k1 k2. k1 -> Either k1 k2
foreign import data Right :: forall k1 k2. k2 -> Either k1 k2

foreign import data Lift1 :: forall k1 k2. (k1 -> k2) -> k1 -> TypeExpr k2

instance Eval (Lift1 f a) (f a)

type MapRow :: forall k1 k2. (k1 -> TypeExpr k2) -> Row k1 -> TypeExpr (Row k2)
type MapRow f = ToRow <<< Map f <<< FromRow

--
-- infixl 10 type RL.Cons as :=
--
-- type Apply :: forall k1 k2. (k1 -> k2) -> k1 -> k2
-- type Apply f a = f a
--
-- infixr 0 type Apply as $
--
-- type X = "test" := Int $ "fest" := String $ RL.Nil

type MarkedElem k1 k2 = Symbol /\ (Either k1 k2)

type Accum k1 k2 = Maybe (Maybe (MarkedElem k1 k2) /\ RowList (k1 /\ k2))

-- | The first parameters are types which should be used as NULL placeholders for a particular kind.
foreign import data Step :: forall k1 k2. k1 -> k2 -> Symbol -> Either k1 k2 -> TypeExpr (Accum k1 k2) -> TypeExpr (Accum k1 k2)

foreign import data OuterJoin :: forall k1 k2. k1 -> k2 -> Row k1 -> Row k2 -> TypeExpr (Row (k1 /\ k2))

foreign import data FinallizeJoin :: forall k1 k2. k1 -> k2 -> Accum k1 k2 -> TypeExpr (RowList (k1 /\ k2))

instance Eval (FinallizeJoin n1 n2 Nothing) RL.Nil
instance Eval (FinallizeJoin n1 n2 (Just (Tuple' Nothing list))) list
instance Eval (FinallizeJoin n1 n2 (Just (Tuple' (Just (Tuple' k (Left v))) list))) (RL.Cons k (Tuple' v n2) list)
instance Eval (FinallizeJoin n1 n2 (Just (Tuple' (Just (Tuple' k (Right v))) list))) (RL.Cons k (Tuple' n1 v) list)

data NULL

type OuterJoin' = OuterJoin NULL NULL

instance
  ( Eval (MapRow (Lift1 Left) r1) r1'
  , Eval (MapRow (Lift1 Right) r2) r2'
  , Eval (Union r1' r2') u
  , Eval ((ToRow <<< FinallizeJoin null1 null2 <<< FoldrWithIndex (Step null1 null2) (Lift Nothing) <<< FromRow) u) u'
  ) =>
  Eval (OuterJoin null1 null2 r1 r2) u'

instance (Eval accum accum', Join null1 null2 key value accum' accum'') => Eval (Step null1 null2 key value accum) accum''

type EmptyAccum = Tuple' Nothing RL.Nil

type AccumConsElem key value list = Just (Tuple' Nothing (RL.Cons key value list))
type AccumStoreElem key value list = Just (Tuple' (Just (Tuple' key value)) list)
type AccumNonElem list = Just (Tuple' Nothing list)

class Join :: forall k1 k2. k1 -> k2 -> Symbol -> Either k1 k2 -> Accum k1 k2 -> Accum k1 k2 -> Constraint
class Join n1 n2 l v acc acc' | n1 n2 l v acc -> acc'

instance Join n1 n2 key value Nothing (AccumStoreElem key value RL.Nil)
else instance Join n1 n2 key value (AccumNonElem list) (AccumStoreElem key value list)

else instance Join n1 n2 key (Left lv) (AccumStoreElem key (Right rv) list) (AccumConsElem key (Tuple' lv rv) list)
else instance Join n1 n2 key (Right rv) (AccumStoreElem key (Left lv) list) (AccumConsElem key (Tuple' lv rv) list)

else instance Join n1 n2 key' v' (AccumStoreElem key (Right rv) tail) (AccumStoreElem key' v' (RL.Cons key (Tuple' n1 rv) tail))
else instance Join n1 n2 key' v' (AccumStoreElem key (Left lv) tail) (AccumStoreElem key' v' (RL.Cons key (Tuple' lv n2) tail))

-- FIXME: drop this
else instance Join n1 n2 key' v acc (Just EmptyAccum)

outerJoin :: forall l o r. Eval (OuterJoin' l r) o => Proxy l -> Proxy r -> Proxy o
outerJoin _ _ = Proxy

type LeftRow = (foo :: Int, bar :: String)
type RightRow = (foo :: Boolean, baz :: String)

z :: Proxy (baz :: Tuple' NULL String, bar :: Tuple' String NULL, foo :: Tuple' Int Boolean)
z = outerJoin (Proxy :: Proxy LeftRow) (Proxy :: Proxy RightRow)
