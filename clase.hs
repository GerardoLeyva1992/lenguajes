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


acomodadorapido []=[]
acomodadorapido (x:xs)=let maschiquito=acomodadorapido [a|a<-xs,a<=x]
			   masgrandote=acomodadorapido [a|a<-xs,a>x]
			in maschiquito ++[x]++ masgrandote

divisiniota =head (filter p [100000,999999..])
	where p x= x `mod` 3829==0
