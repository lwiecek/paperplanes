open Printf
module Geometry =
  
  struct

    (* type orientation = Top | Flat | Bottom ;; *)
    type point = Point of float * float * int ;; (* x,y,level *)
    type paper = Paper of point list ;;
    type intersection = None | Intersection of point;;

    let blankPaper = 
      Paper([
        Point(-1.0, -1.0, 0);
        Point(-1.0, 1.0, 0);
        Point(1.0, 1.0, 0);
        Point(1.0, -1.0, 0)
      ])
    ;;

    (* Iloczyn skalarny punktow a i b *)
    let dotProduct a b =
      let Point(ax, ay, _) = a in
      let Point(bx, by, _) = b in
      ax *. bx +. ay *. by
    ;;

    (*
      Czy linie wyznaczone przez punkty p0,p1 i p2,p3 sie przecinaja? 
      Jesli tak to zwraca dokladny punkt przeciecia
    *)
    let intersects p0 p1 p2 p3 =
      let Point(p0x, p0y, _), Point(p1x, p1y, _) = p0, p1 in
      let Point(p2x, p2y, _), Point(p3x, p3y, _) = p2, p3 in
      let s1x, s1y = p1x -. p0x, p1y -. p0y in
      let s2x, s2y = p3x -. p2x, p3y -. p2y in
      let s = (-.s1y*.(p0x-.p2x) +. s1x*.(p0y-.p2y)) /. (-.s2x*.s1y +. s1x*.s2y) in
      let t = (  s2x*.(p0y-.p2y) -. s2y*.(p0x-.p2x)) /. (-.s2x*.s1y +. s1x*.s2y) in
      if (s >= 0.0 && s <= 1.0 && t >= 0.0 && t <= 1.0) then
        Intersection(Point(p0x +. (t*.s1x), p0y +. (t*.s1y), 0))
      else 
        None
    ;;

    (* Rzutowanie ortogonalne punktu a na prosta wyznaczona przez p0, p1 *)
    let orthoProj p0 p1 a =
      let Point(p0x, p0y, _) = p0 in
      let Point(p1x, p1y, _) = p1 in
      let Point(ax, ay, _) = a in
      let sx = p1x -. p0x in
      let sy = p1y -. p0y in
      let s = Point(sx, sy, 0) in
      let v = Point(ax -. p0x, ay -. p0y, 0) in
      let fact = (dotProduct v s) /. (dotProduct s s) in
      Point(sx *. fact +. p0x, sy *. fact +. p0y, 0)
    ;;

    (* Oblicz wektor normalny *)
    let calcNormal p1x p1y p1z p2x p2y p2z p3x p3y p3z =
      let ax = p2x -. p1x in
      let ay = p2y -. p1y in
      let az = p2z -. p1z in
      let bx = p3x -. p1x in
      let by = p3y -. p1y in
      let bz = p3z -. p1z in

      let nx = (ay *. bz) -. (az *. by) in
      let ny = (az *. bx) -. (ax *. bz) in
      let nz = (ax *. by) -. (ay *. bx) in

      let l = sqrt(nx *. nx +. ny *. ny +. nz *. nz) in
      (nx /. l, ny /. l, nz /. l)
    ;;

    (* Zwraca punkt a odbity wzgledem punktu projekcji *)
    let projDiff a proj =
      let Point(ax, ay, level) = a in
      let Point(projx, projy, _) = proj in
      Point(ax +. 2. *. (projx -. ax), ay +. 2. *. (projy -. ay), level + 1)
    ;;

    (* Sprawdza czy punkt znajduje się po lewej stronie prostej 
       Zwijanie papieru przerzuca punkty z jednej strony na drugą.
    *)
    
    let isLeft p0 p1 c =
      let Point(p0x, p0y, _) = p0 in
      let Point(p1x, p1y, _) = p1 in
      let Point(cx, cy, _) = c in
      ((p1x -. p0x)*.(cy -. p0y) -. (p1y -. p0y)*.(cx -. p0x)) > 0.0
    ;;

    (* Wez kartke papieru 'paper' oraz wspolrzedne odcinka p0, p1 i zwroc nowa,
       zagieta kartke papieru *)
    let foldPaper paper p0 p1 = 
      let rec foldPaperRec points p0 p1 result =
        match points with
          | a::b::t -> 
          begin
            if isLeft p0 p1 a then
              let proj = orthoProj p0 p1 a in
              let a' = projDiff a proj in
              match intersects a b p0 p1 with
                | None -> foldPaperRec (b::t) p0 p1 (a'::result)
                | Intersection(c) ->
                    foldPaperRec (b::t) p0 p1 (c::a'::result)
            else
              match intersects a b p0 p1  with
                | None -> foldPaperRec (b::t) p0 p1 (a::result)
                | Intersection(c) -> 
                  foldPaperRec (b::t) p0 p1 (c::a::result)
          end
          | _ -> Paper(List.rev result)
      in
        let Paper points = paper in
        foldPaperRec (points @ [List.hd points]) p0 p1 []
    ;;

    (* Zwraca liste odcinkow danego kawalka papieru *)
    let paperSegments paper =
      
      let rec paperSegmentsRec points result =
        match points with
          | a::b::t -> paperSegmentsRec (b::t) ((a, b) :: result)
          | _ -> result
      in
        let Paper points = paper in
          paperSegmentsRec points []
      

    (* Zwraca liste trojkatow dla danej, pozaginanej kartki papieru *)
    (* Kartka musi miec co najmniej 3 punkty *)
    let paperTriangles paper =
      (* Zwraca liste punktow podzielona na obszary o wspolnym poziomie *)
      (*
      let rec levels points level result =

        match points with
          | a::b::t -> 
            let Point(ax, ay, alev) = a in
            let Point(bx, by, blev) = b in
              if blev > level then
                let (r, ps) = levels (b::points) blev [a] in
                levels ps level r 
              else
              if alev < level then
                (a::result, points)
              else
                levels (b::points) level result @ [a]

          | [] -> result
      *)
      let rec paperTrianglesRec stack points result level =
        match points with
          | b::c::t -> 
            let Point(cx, cy, clev) = c in
            let Point(bx, by, blev) = b in
            if clev > level then begin              
              
              let s, ps, r = (paperTrianglesRec (b::stack) (c::t) (result) clev) in
                paperTrianglesRec s ps (r @ result) level
            end else if blev < level then begin              
              (List.tl stack, (b::c::t), result)
              (* paperTrianglesRec (List.tl stack) (b::c::t) (result) blev *)
            end else
              (* if clev > level then
                paperTrianglesRec c 
              let Point(cx, cy, clev) = c in
              if clev > level then
                paperTrianglesRec stack (c::t) (result) level
              else *)
                paperTrianglesRec stack (c::t) ((List.hd stack, b, c) :: result) level
          | _ -> ([], [], result)
      in
        let Paper points = paper in    
        let (_, _, r) = paperTrianglesRec [List.hd points] (List.tl points) [] 0 in
        r
      ;;

  end
;;  