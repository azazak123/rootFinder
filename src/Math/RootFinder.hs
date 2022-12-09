module Math.RootFinder where

bisection a b f eps
  | signum (f a) * signum (f b) > 0 = Nothing
  | abs (f middle) < eps = Just middle
  | f a < 0 && f middle > 0 = bisection a middle f eps
  | otherwise = bisection middle b f eps
  where
    middle = (a + b) / 2

iteration x f f' iterLimit eps
  | iterLimit == 0 = Nothing
  | abs (xn - x) < eps = Just xn
  | otherwise = iteration xn f f' (iterLimit - 1) eps
  where
    xn = x - a * f x
    a
      | f' x > 0 = 1 / f' x
      | otherwise = -1 / abs (f' x)

newton x f f' iterLimit eps
  | iterLimit == 0 = Nothing
  | abs (xn - x) < eps = Just xn
  | otherwise = newton xn f f' (iterLimit - 1) eps
  where
    xn = x - f x / f' x

chord a b f eps
  | signum (f a) * signum (f b) > 0 = Nothing
  | abs (x - a) < eps = Just x
  | otherwise = chord x b f eps
  where
    x = a - f a * (b - a) / (f b - f a)
