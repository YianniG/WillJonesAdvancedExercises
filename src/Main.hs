-- Advanced programming exercises.

{-
  (.) :: (a -> b) -> (b -> c) -> (a -> c)

  (.) :: (((q -> w) -> (w -> r)) -> (q -> r)) -> (((s -> d) -> (d -> f)) -> (s
   -> f))
  -> (() -> c)

  (.:) :: (a -> b) ->
  (.:) = (.) . (.)
-}
(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
-- Take give me two args ill put that irst through the double args and the
-- result through the single args.
(.:) = (.) . (.)

sum :: Num a => [a] -> a
sum  = (foldr (+) 0)

any :: (a -> Bool) -> [a] -> Bool
-- any p = (or . (map p))
any = or .: map

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f a b c = flip (f a)

