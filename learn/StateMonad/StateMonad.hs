{-# LANGUAGE InstanceSigs #-}
import Control.Monad


data TurnstileState = Locked | Unlocked
  deriving (Show, Eq)

data TurnstileOutput = Thank | Open | Tut
  deriving (Show, Eq)


coin, push :: TurnstileState -> TurnstileOutput

coin _ = Thank

push Unlocked = Open
push Locked   = Tut

coin', push' :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin' _ = (Thank, Unlocked)

push' Locked = (Tut, Locked)
push' Unlocked = (Open, Locked)


monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin' s0
      (a2, s2) = push' s1
      (a3, s3) = push' s2
      (a4, s4) = coin' s3
      (a5, s5) = push' s4
  in ([a1,a2,a3,a4,a5], s5)

-- Now use States

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
--fmap = liftM -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
  fmap ab ma = State { runState = ( \s0 ->
    let (a,s1) = (runState ma s0)
    in (ab a,s1) )}

instance Applicative (State s) where
--pure = return -- return :: Monad m => a -> m a
  pure :: a -> State s a
  pure a = state ( \s0 -> (a,s0) )


--(<*>) = ap  -- ap :: Monad m => m (a -> b) -> m a -> m b
  (<*>) mab ma = State { runState = (\s0 ->
    let (f,s1) = runState mab s0
        (a,s2) = runState ma s1
    in (f a, s2) ) }


instance Monad (State s) where
  return :: a -> State s a
  return x = state ( \ s -> (x, s) )

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) sa asb = State {
    runState = ( \s0 ->
      let (a, s1) = runState sa s0
          (b, s2) = runState (asb a) s1
       in (b, s2) ) } 
{-
  (>>=) p k = q where
    p' = runState p      -- p' :: s -> (a, s)
    k' = runState . k    -- k' :: a -> s -> (b, s)
    q' s0 = (y,s2) where -- q' :: s -> (b, s)
      (x, s1) = p' s0
      (y, s2) = k' x s1
    q = state q'
-}

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin'
pushS = state push'



mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1,a2,a3,a4,a5]

mondayS' :: State TurnstileState [TurnstileOutput]
mondayS' = 
  coinS >>= (\ a1 ->
    pushS >>= (\ a2 ->
      pushS >>= (\ a3 ->
        coinS >>= (\ a4 ->
          pushS >>= (\ a5 ->
            return [a1,a2,a3,a4,a5] )))))

tuesdayS' :: State TurnstileState [TurnstileOutput]
tuesdayS' =
  coinS >>= (\ a1 ->
    return [a1] )

mondayS'' :: State TurnstileState [TurnstileOutput]
mondayS'' = sequence [coinS, pushS, pushS, coinS, pushS]




compose :: (s -> (a,s)) ->
           (a -> ( s -> (b,s) )) ->
           (s -> (b,s))
compose f g = \s0 -> let (a1,s1) = f s0 in (g a1) s1









