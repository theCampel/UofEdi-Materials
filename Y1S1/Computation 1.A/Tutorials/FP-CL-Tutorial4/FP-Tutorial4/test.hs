data Shape = Circle Float | Rect Float Float

eqShape :: Shape -> Shape -> Bool
eqShape (Circle r) (Circle r')   =  (r == r')
eqShape (Rect w h) (Rect w' h')  =  (w == w') && (h == h')
eqShape x y  =  False