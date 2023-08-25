-- Exercises 1

{--
1. Express the following angles in radians.
(a). 12 degrees, 28 minutes, that is, 12° 28'.
(b). 36° 12'.
-}

minutesToDecimal :: Fractional a => a -> a -> a -> a
minutesToDecimal d m s = d + m / 60 + s / 3600

angleToRadians :: Floating a => a -> a -> a -> a
angleToRadians d m s = minutesToDecimal d m s * pi / 180

{--
2. Reduce the following numbers of radians to degrees, minutes, and seconds.
(a). 0.47623.
(b). 0.25412.
--}

degreeConverter :: (Floating a, RealFrac a) => a -> [a]
degreeConverter d = [degree, minute, second]
  where
    decimaldegree = d * 180 / pi
    degree = fromIntegral (truncate decimaldegree)
    minute = snd (properFraction decimaldegree) * 60
    second = snd (properFraction minute) * 60

{--
3. Given the angle a and the radius r, to find the length of the subtending arc.
(a). a = 0° 17' 48", r = 6.2935.
(b). a = 121° 6' 18", r = 0.2163.
--}
arcFinder :: Floating a => a -> a -> a -> a -> a
arcFinder d m s r = angleToRadians d m s * r

{--
4. Given the length of the arc l and the radius r, to find the angle subtended at the center.
(a). l = .16296, r = 12.587.
(b). l = 1.3672, r = 1.2978.
--}

angleFinder :: (Floating a, RealFrac a) => a -> a -> [a]
angleFinder l r = degreeConverter (l / r)

{--
5. Given the length of the arc l and the angle a which it subtends at the center, to find the radius.
(a). a = 0° 44' 30", l = .032592.
(b). a = 60° 21' 6", l = .4572.
--}

radiusFinder :: Floating a => a -> a -> a -> a -> a
radiusFinder d m s l = l / angleToRadians d m s

{--
6. Find the length to the nearest inch of a circular arc of 11 degrees 48.3 minutes if the radius is 3200 feet.
--}
