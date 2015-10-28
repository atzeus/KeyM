
{-# LANGUAGE GADTs, KindSignatures, DataKinds,TypeOperators,Rank2Types,GeneralizedNewtypeDeriving, DeriveFunctor #-}
import Data.Typeable
import Data.Type.Equality
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.State
import Control.Monad.Trans
import Data.List
data T a where
  T :: Typeable a => T a

instance TestEquality T where
  testEquality T T = eqT

data Cons h t
data Nil

data TList f l where
  Nil :: TList f '[]
  HT  :: f h -> TList f t -> TList f (h ': t)

ffmap :: (forall x. f x -> g x) -> TList f ls -> TList g ls
ffmap f Nil = Nil
ffmap f (HT h t) = HT (f h) (ffmap f t)

data Key l a where
  Head :: Key (h ': t) h
  Tail :: Key t a -> Key (h ': t) a

getKey :: TList T ls -> T a -> Integer -> Key ls a
getKey (HT h _) v 0 
  | Just Refl <- testEquality h v = Head
getKey (HT _ t) v n = Tail $ getKey t v (n - 1)

instance TestEquality (Key l) where
  testEquality Head Head = Just Refl
  testEquality (Tail t) (Tail t') = testEquality t t'
  testEquality _ _ = Nothing


type KeyM s a =  ReaderT (TList T s) ( WriterT [Ex T] (State Integer)) a

newKey :: T a -> KeyM s (Key s a)
newKey t = do lift (tell [Ex t])
              f <- ask
              i <- lift $ lift get
              lift  $ lift $ put (i + 1)
              return (getKey f t i)


runKeyM :: (forall s. KeyM s a) -> a
runKeyM m = 
  let (a,ts) = runIt m (toExT ts)
  in a

runIt :: (forall s. KeyM s a) -> ExL (TList T) -> (a, [Ex T])
runIt m (ExL x) =  evalState (runWriterT $ runReaderT m x) 0



toExT :: [Ex f] -> ExL (TList f) 
toExT [] = ExL Nil
toExt (h : t) = ht h (toExt t)

ht :: Ex f -> ExL (TList f) -> ExL (TList f)
ht (Ex h) (ExL t) = ExL $ HT h t

data Ex (f :: * -> *)   where
  Ex :: f a -> Ex f

data ExL (f :: [*] -> *)   where
  ExL :: f a -> ExL f

