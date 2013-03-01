open GL
open Glu
open Glut
open Printf

type orientation = Top | Flat | Bottom ;;
type point = Point of float * float * orientation ;;
type paper = Paper of point list ;;
type intersection = None | Intersection of point;;

let blankPaper = 
  Paper([
    Point(-1.0, -1.0, Flat);
    Point(-1.0, 1.0, Flat);
    Point(1.0, 1.0, Flat);
    Point(1.0, -1.0, Flat)
  ])
;;

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
    Intersection(Point(p0x +. (t*.s1x), p0y +. (t*.s1y), Flat))
  else 
    None
;;

(* Wez kartke papieru 'paper' oraz wspolrzedne odcinka p0, p1 i zwroc nowa,
   zagieta kartke papieru *)
let foldPaper paper p0 p1 orientation = 
  let rec foldPaperRec paper p0 p1 result =
    match paper with
      | [] -> result
      | a::t -> result
  in
    foldPaperRec paper p0 p1 []
;;

(* Zwraca liste trojkatow dla danej, pozaginanej kartki papieru *)
(* Kartka musi miec co najmniej 3 punkty *)
let paperTriangles paper =
  let rec paperTrianglesRec points result =
    match points with
      | a::b::c::t -> paperTrianglesRec (b::c::t) ((a, b, c) :: result)
      | _ -> result
  in
    let Paper p = paper in
    let fst, snd = List.hd p, List.hd (List.tl p) in
      paperTrianglesRec (p @ [snd] @ [fst]) []

let displayPaper paper =
  glColor3 0.5 0.5 0.5;
  glBegin GL_TRIANGLES;
  let tri points = 
    let Point(ax, ay, _), Point(bx, by, _), Point(cx, cy, _) = points in
    glVertex2 ax ay;
    glVertex2 bx by;
    glVertex2 cx cy;
  in
    List.iter tri (paperTriangles paper);
  glEnd()
;;

(* Polozenie myszy na ekranie *)
let mousex = ref 0.0
let mousey = ref 0.0

(* Wspolrzedne ostatniego klikniecia *)
let clickx = ref 0.0
let clicky = ref 0.0

(* try to force refresh every "min_refresh" milliseconds *)
let min_refresh = 30

(* Wpolrzende ograniczajace okno *)
let x_min = -1.2 and x_max = 1.2
and y_min = -1.2 and y_max = 1.2
and z_min, z_max = -6.0, 60.0 ;;

(* Lewy przycisk myszy *)
let lmousedown = ref false

(* Prawy przysik myszy *)
(* Przy wcisnietym prawym przycisku można obracać model
   Po puszczeniu wraca on do widoku edycji z lotu ptaka - zaginanie kartki *)
let rmousedown = ref false

(* Czy klawisz shift jest nacisniety? *)
(* Jesli tak to podczas edycji modelu kartka bedzie zaginana do srodka.
   W przeciwnym przypadku kartka zaginana jest na zewnatrz *)
let shift_key = ref false

(* Katy obrotu w przypadku nacisniecia prawego przycisku myszy *)
let angley = ref 0
let anglex = ref 0

let paper = ref blankPaper



let display() =
  glClear [GL_COLOR_BUFFER_BIT];
  glLoadIdentity();

  
  glPointSize 5.0;
  if !rmousedown then
  begin
    glRotate ~angle:(float(- !angley)) ~x:1.0 ~y:0.0 ~z:0.0;
    glRotate ~angle:(float(- !anglex)) ~x:0.0 ~y:1.0 ~z:0.0;
    displayPaper !paper;
  end
  else
  begin
    displayPaper !paper;
    if !lmousedown then
    begin
      glBegin GL_POINTS;
      glColor3 1. 1. 0.;
      glVertex2 !mousex !mousey;
      glVertex2 !clickx !clicky;
      glEnd();
      if !shift_key then
        glColor3 1. 0. 0.
      else
        glColor3 0.2 0.2 1.;

      glBegin GL_LINES;
        glVertex2 ~x:!clickx ~y:!clicky;
        glVertex2 ~x:!mousex ~y:!mousey;
      glEnd();
    end
    else
    begin
      glBegin GL_POINTS;
      glColor3 0. 1. 0.;
      (* Narysuj kursor myszy *)
      glVertex2 !mousex !mousey;
      glEnd();
    end;

  end;
  glFlush();
  glutSwapBuffers();
;;

(*
let display() =
  glClear [GL_COLOR_BUFFER_BIT];
  glLoadIdentity();
  glColor3 ~r:0. ~g:1.0 ~b:0.;
  glBegin GL_LINES;
    glVertex2 ~x:!xcur ~y:!ycur;
    glVertex2 ~x:!xold ~y:!yold;
  glEnd();
  glutWireCube ~size:!xcur;
  (*
  glRotate ~angle:(float(- !angley)) ~x:1.0 ~y:0.0 ~z:0.0;
  glRotate ~angle:(float(- !anglex)) ~x:0.0 ~y:1.0 ~z:0.0;
  glColor3 ~r:0. ~g:1.0 ~b:0.;
  glutWireCube ~size:1.0;
  *)
  glFlush();
  glutSwapBuffers();
;;
*)

(*
(* active mouse motion *)
let motion ~x ~y =
  if !b_down then  (* if the left button is down *)
  begin

 (* change the rotation angles according to the last position
    of the mouse and the new one *)
    (*
    anglex := !anglex + (!xold - x);
    angley := !angley + (!yold - y);
  *)
    glutPostRedisplay();
  end;
  
  xcur := float_of_int(x);  (* save mouse position *)
  ycur := float_of_int(y);
  
;;
*)

(* convert the coordinates of the mouse
   from window coordinates to the local
   representation *)
let reg_unproject_coords ~x ~y =
  let mx, my, _ = gluUnProjectUtil ~x ~y in
  mousex := mx;
  mousey := my;
;;

(* active mouse motion *)
let motion ~x ~y =
  reg_unproject_coords ~x ~y;  
  
;;

(* passive mouse motion *)
let passive ~x ~y =
  reg_unproject_coords ~x ~y;
;;

(* mouse button event *)
let mouse ~button ~state ~x ~y =
  reg_unproject_coords ~x ~y;
  shift_key := List.exists (fun x -> x = GLUT_ACTIVE_SHIFT) (glutGetModifiers()); 
  match button, state with
  | GLUT_LEFT_BUTTON, GLUT_DOWN ->
      lmousedown := true;
      let mx, my, _ = gluUnProjectUtil ~x ~y in
        clickx := mx;
        clicky := my;
  | GLUT_LEFT_BUTTON, GLUT_UP -> lmousedown := false;
  | GLUT_RIGHT_BUTTON, GLUT_DOWN -> rmousedown := true;
  | GLUT_RIGHT_BUTTON, GLUT_UP -> rmousedown := false;
  | _ -> ()
;;

(*
(* mouse button event *)
let mouse ~button ~state ~x ~y =
  match button, state with
  (* if we press the left button *)
  | GLUT_LEFT_BUTTON, GLUT_DOWN ->
      b_down := true;
      xold := float_of_int(x);  (* save mouse position *)
      yold := float_of_int(y);
  (* if we release the left button *)
  | GLUT_LEFT_BUTTON, GLUT_UP ->
      b_down := false;
  | _ -> ()
;;
*)

let keyboard ~key ~x ~y =
  match key with
  | '\027' (* escape key *)
  | 'q' -> exit 0
  | _ -> ()

;;

let reshape  ~width:w ~height:h =
  glViewport 0 0 w h;
  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  if w <= h then
    glOrtho x_min x_max (y_min *. float h /. float w)
                        (y_max *. float h /. float w) z_min z_max
  else
    glOrtho (x_min *. float w /. float h)
            (x_max *. float w /. float h) y_min y_max z_min z_max;

  glMatrixMode GL_MODELVIEW;
  glLoadIdentity();
;;

(*
let reshape ~width:w ~height:h =
  glViewport 0 0 w h;
  let aspect = (float w) /. (float h) in
  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  gluPerspective ~fovy:60.0 ~aspect ~zNear:0.5 ~zFar:80.0;
  glMatrixMode GL_MODELVIEW;
;;
*)

let idle () =
  glutPostRedisplay();
;;

let gl_init() =
  glClearColor 0.5 0.5 0.5 0.0;
  glShadeModel GL_FLAT;
;;

let () =
  ignore(glutInit Sys.argv);
  glutInitDisplayMode [GLUT_DOUBLE];
  glutInitWindowSize 800 600;
  glutInitWindowPosition 100 100;
  ignore(glutCreateWindow ~title:Sys.argv.(0));
  glutSetCursor GLUT_CURSOR_NONE;
  glutDisplayFunc ~display;
  glutReshapeFunc ~reshape;
  glutIdleFunc ~idle;
  glutMouseFunc ~mouse;
  glutKeyboardFunc ~keyboard;
  glutMotionFunc ~motion;
  glutPassiveMotionFunc ~passive;
  glutMainLoop();
;;

