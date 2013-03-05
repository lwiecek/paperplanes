module Geometry :
  sig
    type point = Point of float * float * int
    type paper = Paper of point list
    type intersection = None | Intersection of point
    val blankPaper : paper
    val dotProduct : point -> point -> float
    val intersects : point -> point -> point -> point -> intersection
    val orthoProj : point -> point -> point -> point
    val calcNormal :
      float ->
      float ->
      float ->
      float ->
      float -> float -> float -> float -> float -> float * float * float
    val projDiff : point -> point -> point
    val isLeft : point -> point -> point -> bool
    val foldPaper : paper -> point -> point -> paper
    val paperSegments : paper -> (point * point) list
    val paperTriangles : paper -> (point * point * point) list
  end
