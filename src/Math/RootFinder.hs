module Math.RootFinder where

data ChordMethodType = FixLeft | FixRight

bisection a b f iterLimit eps
  | signum (f a) * signum (f b) > 0 = Nothing
  | iterLimit == 0 = Nothing
  | bias < eps = Just (middle, bias)
  | f a <= 0 && f middle >= 0 || f a >= 0 && f middle <= 0 = bisection a middle f (iterLimit - 1) eps
  | otherwise = bisection middle b f (iterLimit - 1) eps
  where
    middle = (a + b) / 2
    bias = abs (middle - b)

iteration a b x f f' iterLimit eps
  | x < a || x > b = Nothing
  | signum (f a) * signum (f b) > 0 = Nothing
  | iterLimit == 0 = Nothing
  | bias < eps = Just (xn, bias)
  | otherwise = iteration a b xn f f' (iterLimit - 1) eps
  where
    xn = x - coef * f x
    bias = abs (f xn) / minimumOnRange a b f' eps
    coef
      | f' x > 0 = 2 / (minimumOnRange a b f' eps + maximumOnRange a b f' eps)
      | otherwise = -2 / (minimumOnRange a b f' eps + maximumOnRange a b f' eps)

newton a b x f f' iterLimit eps
  | x < a || x > b = Nothing
  | signum (f a) * signum (f b) > 0 = Nothing
  | iterLimit == 0 = Nothing
  | bias < eps = Just (xn, bias)
  | otherwise = newton a b xn f f' (iterLimit - 1) eps
  where
    xn = x - f x / f' x
    bias = abs (f xn) / minimumOnRange a b f' eps

chord a b f f' fix iterLimit eps
  | iterLimit == 0 = Nothing
  | signum (f a) * signum (f b) > 0 = Nothing
  | otherwise = chord' a b (minimumOnRange a b f' eps) f fix iterLimit eps

chord' a b m f FixRight iterLimit eps
  | iterLimit == 0 = Nothing
  | signum (f a) * signum (f b) > 0 = Nothing
  | bias < eps = Just (x, bias)
  | otherwise = chord' x b m f FixRight (iterLimit - 1) eps
  where
    x = a - f a * (b - a) / (f b - f a)
    bias = abs (f x) / m
chord' a b m f FixLeft iterLimit eps
  | iterLimit == 0 = Nothing
  | signum (f a) * signum (f b) > 0 = Nothing
  | bias < eps = Just (x, bias)
  | otherwise = chord' a x m f FixLeft (iterLimit - 1) eps
  where
    x = b - f b * (b - a) / (f b - f a)
    bias = abs (f x) / m

minimumOnRange a b f eps = minimum . fmap (abs . f . (*) eps) $ [a / eps .. b / eps]

maximumOnRange a b f eps = maximum . fmap (abs . f . (*) eps) $ [a / eps .. b / eps]
