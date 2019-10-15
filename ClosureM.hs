-- NOTE: Store a partially applied function in a container
--       So that we can treat it as if it already returned?
--       We can also feed in the arguments under the hood (prevent changes
--       midway/middleman?)
-- NOTE: Maybe the difference is that this time the user isn't allowed to
--       "know" about the state, but only adding things to it? <- Writer behavior
--       Also the fact that the "read in value" isn't changed per se; all calls
--       will use a copy of the read in value in their computation, instead of
--       like in State, we will thread a (potentially) changing state throughout
--       the calls

module ClosureM
( ClosureM(..),
  pickConstraint,
  emit,
  runClosureM
)
where
import AST
import Data.Monoid
import qualified Data.Set as S

type ConstraintSet = S.Set Constraint

data ClosureM x = ClosureM (ConstraintSet -> [(x, ConstraintSet)])

instance Functor ClosureM where
  fmap f (ClosureM x) = ClosureM (\r -> let lst = x r in (map (\p -> (f (fst p), snd p)) lst))

instance Applicative ClosureM where
  pure x = ClosureM (\r -> [(x, mempty)])
  -- (<*>) :: (Functor f) => f (a -> b) -> f a -> f b
  ClosureM x <*> ClosureM y =
    ClosureM (\r -> let lstf = x r in
                let lstY = y r in
                [((fst fm) (fst vm), (snd fm) <> (snd vm)) | fm <- lstf, vm <- lstY])

instance Monad ClosureM where
  return x = ClosureM (\r -> [(x, mempty)])
  -- (>>=) :: ClosureM r w a -> (a -> ClosureM r w b) -> ClosureM r w b
  cur >>= f =
    ClosureM (\r -> let ClosureM x = cur in
                let lstX = x r in
                -- NOTE: lstX :: list of (v, w) pairs
                let lstM = [ let ClosureM resM = f (fst a) in
                             let res = (resM r) in
                             [(fst resA, (snd resA) <> (snd a)) | resA <- res] | a <- lstX ]
                in concat lstM)

pickConstraint :: () -> ClosureM Constraint
pickConstraint () = ClosureM (\r -> let consLst = S.toList r in
                                    [(cons, mempty) | cons <- consLst])

emit :: Constraint -> ClosureM ()
emit cons = ClosureM (\r -> [((), S.singleton cons)])

runClosureM :: ConstraintSet -> ClosureM a -> ([a], ConstraintSet)
runClosureM cset x =
  let ClosureM rf = x in
  let lst = rf cset in
  let lstA = map fst lst in
  let lstW = map snd lst in
  let fW = mconcat lstW in
  (lstA, fW)
