module Geometry :
  sig
    type orientation = Top | Flat | Bottom
    type point = Point of float * float * orientation
    type paper = Paper of point list
    type intersection = None | Intersection of point
    val dotProduct : point -> point -> float
    val intersects : point -> point -> point -> point -> intersection
    val orthoProj : point -> point -> point -> point
    val calcNormal :
      float ->
      float ->
      float ->
      float ->
      float -> float -> float -> float -> float -> float * float * float
  end