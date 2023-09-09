module Ch5.Gcd where
import Control.Monad.Writer

gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

gcdM :: (Integral a, Monad m) => (a -> a -> m ()) -> a -> a -> m a
gcdM step a 0 = step a 0 >> pure a
gcdM step a b = step a b >> gcdM step b (a `mod` b)

gcdLogSteps :: Integral a => a -> a -> Writer [(a,a)] a
gcdLogSteps a b = gcdM (\a b -> tell [(a, b)]) a b

gcdCountSteps :: Integral a => a -> a -> Writer (Sum Int) a
gcdCountSteps a b = gcdM (\_ _ -> tell $ Sum 1) a b