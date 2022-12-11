module Math.RootFinder where

data ChordMethodType = FixLeft | FixRight

bisection a b f iterLimit eps
  | signum (f a) * signum (f b) > 0 = Nothing
  | iterLimit == 0 = Nothing
  | abs (f middle) < eps = Just middle
  | f a < 0 && f middle > 0 = bisection a middle f (iterLimit - 1) eps
  | otherwise = bisection middle b f (iterLimit - 1) eps
  where
    middle = (a + b) / 2

iteration a b x f f' iterLimit eps
  | x < a || x > b = Nothing
  | iterLimit == 0 = Nothing
  | abs (xn - x) < eps = Just xn
  | otherwise = iteration a b xn f f' (iterLimit - 1) eps
  where
    xn = x - a * f x
    a
      | f' x > 0 = 2 / (minimumOnRange a b f' eps + maximumOnRange a b f' eps)
      | otherwise = -2 / (minimumOnRange a b f' eps + maximumOnRange a b f' eps)

newton x f f' iterLimit eps
  | iterLimit == 0 = Nothing
  | abs (xn - x) < eps = Just xn
  | otherwise = newton xn f f' (iterLimit - 1) eps
  where
    xn = x - f x / f' x

chord a b f FixRight iterLimit eps
  | iterLimit == 0 = Nothing
  | signum (f a) * signum (f b) > 0 = Nothing
  | abs (x - a) < eps = Just x
  | otherwise = chord x b f FixRight (iterLimit - 1) eps
  where
    x = a - f a * (b - a) / (f b - f a)
chord a b f FixLeft iterLimit eps
  | iterLimit == 0 = Nothing
  | signum (f a) * signum (f b) > 0 = Nothing
  | abs (x - b) < eps = Just x
  | otherwise = chord a x f FixLeft (iterLimit - 1) eps
  where
    x = b - f b * (b - a) / (f b - f a)

minimumOnRange a b f eps = minimum . fmap (abs . f . (*) eps) $ [a / eps .. b / eps]

maximumOnRange a b f eps = maximum . fmap (abs . f . (*) eps) $ [a / eps .. b / eps]
