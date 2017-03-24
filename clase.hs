aplytwice f x = f (f x)

invert x = -1*x

dupli x = x ++ x





zipCon _ [] _ = []
zipCon _ _ [] = []

zipCon f (x:xs) (y:ys)= [f x y ] ++ (zipCon f xs ys) 

map3Turbo _ [] =[]
map3Turbo f (x:xs)=[f x] ++ (map3Turbo f xs)


filtrado _ [] =[]
filtrado f (x:xs)=if (f x ) then x:(filtrado f xs) else (filtrado f xs)


filtradotypes _ [] =[]
filtradotypes f (x:xs) | f x =x:filtradotypes f xs
	      |otherwise = filtradotypes f xs 	




