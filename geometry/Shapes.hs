module Shapes
where
	
data Shape = Rectangle Side Side |
			       Ellipse Radius Radius |
			       RtTriangle Side Side |
             Polygon [Vertex]
             deriving Show

type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

area :: Shape -> Float
area (Rectangle s1 s2)       = s1 * s2
area (Ellipse r1 r2)         = pi * r1 * r2
area (RtTriangle s1 s2)      = 0.5 * s1 * s2	
area (Polygon (v1:v2:v3:vs)) = triArea'v v1 v2 v3 + area ( Polygon (v1:v3:vs) )
area (Polygon _ )            = 0

triArea :: Side -> Side -> Side -> Float
triArea a b c = let s = 0.5 * (a+b+c)
                in sqrt (s*(s-a)*(s-b)*(s-c))

triArea'v :: Vertex -> Vertex -> Vertex -> Float
triArea'v v1 v2 v3 = let a = distanceBetween v1 v2
                         b = distanceBetween v2 v3
                         c = distanceBetween v3 v1
                     in triArea a b c                 
                
distanceBetween :: Vertex -> Vertex -> Float
distanceBetween (x1,y1) (x2,y2) = sqrt ( (x1-x2)^2 + (y1-y2)^2 ) 