module TypecheckM
( TCM,
  retError,
  runTCM,
  nextTVar
  ) where

import AST as A

data TCMResult a = Failure String | Success a
data TCM a = TCM (Int -> TCMResult (a, Int))

-- TCM contains either an error or a stateful thing
instance Functor TCM where
  fmap f (TCM sf) =
    TCM (\s ->
            let res = sf s in
            case res of
              Failure str -> Failure str
              Success (cont, int) -> Success (f cont, int))

instance Applicative TCM where
  pure x = TCM (\s -> Success (x, s))
  (TCM sf) <*> (TCM sa) =
    TCM (\s -> let res = sf s in
                case res of
                  Failure str -> Failure str
                  Success (f, ns) ->
                    let TCM ret = fmap f (TCM sa) in ret ns)

instance Monad TCM where
  return x = TCM (\s -> Success (x, s))

  -- (>>=) :: TCM a -> (a -> TCM b) -> TCM b
  cur >>= f =
    TCM (\s -> let TCM tcm = cur in
                let temp = tcm s in
                case temp of
                  Failure str -> Failure str
                  Success (v, ns) -> let TCM nvf = f v in nvf ns)

-- To get the functionality of Left in Either
retError :: String -> TCM a
retError str = TCM (\s -> Failure str)
runTCM :: TCM a -> Either String a
runTCM (TCM f) =
  let res = f 0 in
  case res of Success (v, ns) -> Right v
              Failure str -> Left str

-- Reasoning: We want this monad to do everything that state can do
setState :: Int -> TCM ()
setState newState = TCM (\s -> Success ((), newState))
getState :: () -> TCM Int
getState _ = TCM (\s -> Success (s, s))

nextTVar :: () -> TCM Fbtype
nextTVar () = do
    curS <- getState ()
    setState (curS + 1)
    return (TVar $ "a" ++ show(curS))
-- Check this
-- runState :: TCM a -> Int -> TCM a
-- runState sfun s = case sfun of Failure str -> retError str
--                                Success fun -> let (val, newS) = fun s
--                                               in Success \s -> (val, newS)
