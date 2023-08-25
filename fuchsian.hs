import Data.Complex

data Matrix t = Matrix t t t t deriving (Show)

mobius :: (RealFloat a) => Matrix (Complex a) -> Complex a -> Complex a
mobius (Matrix a b c d) z = (a*z + b)/(c*z + d)

invert :: (Num t) => Matrix t -> Matrix t
invert (Matrix a b c d) = (Matrix d  (-b) (-c) a)

class Colouring a where
     neighbour :: (Fractional b) => a -> [(a, b)] -- Send a colour to a list of pairs (colour, probability)
     propagate :: (RealFloat b) => Complex b -> a -> Complex b -- Apply a colour rule to get a new metric space point

data Colour = X | Y | X' | Y' deriving (Show)

instance Colouring Colour where
     neighbour X = [(Y, 1/4), (X', 1/4), (Y', 1/4)]
     neighbour Y = [(X, 1/4), (X', 1/4), (Y', 1/4)]
     neighbour X' = [(X, 1/4), (Y, 1/4), (Y', 1/4)]
     neighbour Y' = [(X, 1/4), (Y, 1/4), (X', 1/4)]

     propagate z c =
          case c of
               X -> mobius (Matrix s (s*cos theta) (s*cos theta) s) z
               Y -> mobius (Matrix (s*(1 + cos theta)) 0 0 (s*(1 - cos theta))) z
               X' -> mobius (invert (Matrix s (s*cos theta) (s*cos theta) s)) z
               Y' -> mobius (invert (Matrix (s*(1 + cos theta)) 0 0 (s*(1 - cos theta)))) z
          where theta = pi/4
                s = 1/sin theta


apply :: (Colouring col, RealFloat a) => Complex a -> [col] -> Complex a
apply z [] = z
apply z (c:cs) = propagate (apply z cs) c
