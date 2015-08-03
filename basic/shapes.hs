type Radius = Float
type Side = Float
type Vertex = (Float, Float)
data Shape = 
  Rectangle Side Side | 
  Ellipse Radius Radius |
  RtTriangle Side Side | 
  Polygon [Vertex] deriving Show