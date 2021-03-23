quadroots a b c | a==0 = error "Not quadratic"
      | discriminant==0 = "Root is " ++ show centre
      | discriminant<0 = "Root is complex"
      | otherwise = "Upper Root is "
         ++ show (centre + offset)
         ++ " and Lower Root is "
         ++ show (centre - offset)
  where discriminant = abs(b*b-4*a*c)
        centre = -b/2*a
        offset = sqrt(discriminant)/2*a