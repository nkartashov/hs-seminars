module CheatSheet where

import Prelude (Bool(True, False), (.), (+), (-), (==), (*), ($), flip, const, undefined, map)
import Data.Function (fix)

true = \x y -> x
false = \x y -> y

unChurchB b = b True False

ifC b = b
not v = ifC v false true
and x y = ifC x y false
or x y = ifC x true y

pair x y f = f x y
fst p = p const
snd p = p $ flip const

unChurchN n = n (+1) 0
zero = \f x -> x
isZero n = n (const false) true
one = succ zero
two = succ one
three = succ two
four = succ three
succ n = \f -> f . n f
plus a b = \f -> a f . b f
plus' a b = a succ b
mult a b = b . a
exp a b = b a

prim_rec b z n = let -- bottom :: (Nat, a)
                     bottom = pair zero z
                     -- b :: Nat -> a -> a
                     cycle_step p = pair (succ $ fst p) $ p b
                     in snd $ n cycle_step bottom

pred = prim_rec const zero
fac = prim_rec (\i -> mult (succ i)) one

minus' a b = b pred a

unChurchL l = l (:) []
nil = \c x -> x
cons a as = \c x -> c a (as c x)
isEmpty l = l (\_ _ -> false) true
head l = l const undefined
prim_recL b z l = let -- bottom :: ([a], b)
                      bottom = pair nil z
                      -- b :: a -> [a] -> b -> b
                      cycle_step a p = pair (cons a (fst p)) (p (b a))
                      in snd $ l cycle_step bottom

tail = prim_recL (\_ as _ -> as) nil
append l a = prim_recL (\a' _ t -> cons a' t) (cons a nil) l
append' l a = l cons (cons a nil)
catenate l r = prim_recL (\a _ t -> cons a t) r l
catenate' l r = l cons r
length = prim_recL (\_ _ a -> succ a) zero
reverse = prim_recL (\a _ t -> append t a) nil
