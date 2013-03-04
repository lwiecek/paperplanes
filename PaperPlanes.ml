open GL
open Glu
open Glut
open Printf
open Geometry

let blankPaper = 
  Geometry.Paper([
    Geometry.Point(-1.0, -1.0, Geometry.Flat);
    Geometry.Point(-1.0, 1.0, Geometry.Flat);
    Geometry.Point(1.0, 1.0, Geometry.Flat);
    Geometry.Point(1.0, -1.0, Geometry.Flat)
  ])
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
and z_min, z_max = -6.0, 60.0

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
let angley = ref 0.0
let anglex = ref 0.0

(* Zacznij z czysta kartka papieru, kwadratem 1x1 *)
let paper = ref blankPaper

;;
  

(* Wez kartke papieru 'paper' oraz wspolrzedne odcinka p0, p1 i zwroc nowa,
   zagieta kartke papieru *)
let foldPaper paper p0 p1 orientation = 
  let rec foldPaperRec points p0 p1 result =
    match points with
      | a::b::t -> 
      begin
        match Geometry.intersects a b p0 p1 with
          | Geometry.None -> foldPaperRec (b::t) p0 p1 (a::result)
          | Geometry.Intersection(c) -> foldPaperRec (b::t) p0 p1 (a::c::result)
      end
      | _ -> Geometry.Paper(List.rev result)
  in
    let Geometry.Paper points = paper in
    let fst, snd = List.hd points, List.hd (List.tl points) in
      foldPaperRec (points @ [fst] @ [snd]) p0 p1 []
;;


(* Zwraca liste trojkatow dla danej, pozaginanej kartki papieru *)
(* Kartka musi miec co najmniej 3 punkty *)
let paperTriangles paper =
  let rec paperTrianglesRec points result =
    match points with
      | a::b::c::t -> paperTrianglesRec (b::c::t) ((a, b, c) :: result)
      | _ -> result
  in
    let Geometry.Paper points = paper in
    let fst, snd = List.hd points, List.hd (List.tl points) in
      paperTrianglesRec (points @ [fst] @ [snd]) []

let displayPaper paper =
  
  glBegin GL_TRIANGLES;
  let tri points = 
    let
      Geometry.Point(ax, ay, _),
      Geometry.Point(bx, by, _), 
      Geometry.Point(cx, cy, _) = points in
      let (nx, ny, nz) = Geometry.calcNormal cx cy 0. bx by 0. ax ay 0. in
        glNormal3 ~nx:nx ~ny:ny ~nz:nz;
      
      glVertex2 ax ay;      

      glVertex2 bx by;

      glVertex2 cx cy;
      
  in
    List.iter tri (paperTriangles paper);
  glEnd();
  let Geometry.Paper points = paper in
  let sph point =
    glPushMatrix();
    let Geometry.Point(x,y,_) = point in
    glTranslate ~x:x ~y:y ~z:0.0;
    glutSolidSphere ~radius:0.05 ~slices:10 ~stacks:10;
    glPopMatrix();
  in
    List.iter sph points;

;;

let display() =
  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
  glLoadIdentity();

  glColor3 0.5 0.5 0.5;
  glPointSize 5.0;
  if !rmousedown then
  begin
    
    glRotate ~angle:(!anglex) ~x:0.0 ~y:1.0 ~z:0.0;  
    glRotate ~angle:(-. !angley) ~x:(1.0) ~y:0.0 ~z:0.0;
    displayPaper !paper;

  end
  else
  begin
    displayPaper !paper;
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
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
    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
  end;
  glFlush();
  glutSwapBuffers();
;;

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
  (*
  printf ("%f" ^^ " %f\n") !clickx !clicky;
  printf ("%f" ^^ " %f\n") !mousex !mousey;
  *) 
  anglex := -. (!mousex -. !clickx) *. 10.0;
  angley := -. (!mousey -. !clicky) *. 10.0;
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
  | GLUT_LEFT_BUTTON, GLUT_UP -> 
      lmousedown := false;
      let p0, p1 = Geometry.Point(!clickx, !clicky, Geometry.Top),
                   Geometry.Point(!mousex, !mousey, Geometry.Top) in
        paper := foldPaper (!paper) p0 p1 Geometry.Top;
  | GLUT_RIGHT_BUTTON, GLUT_DOWN ->
      rmousedown := true;
      let mx, my, _ = gluUnProjectUtil ~x ~y in
        clickx := mx;
        clicky := my;
  | GLUT_RIGHT_BUTTON, GLUT_UP -> 
    rmousedown := false;
    anglex := 0.0;
    angley := 0.0;
  | _ -> ()
;;

(* Wyjscie klawiszem ESC lub q *)
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

let idle () =
  glutPostRedisplay();
;;

let gl_init() =
  glClearColor 0.0 0.0 0.0 0.0;
  glShadeModel GL_FLAT;
  
  glMaterial ~face:GL_FRONT ~mode:(Material.GL_SPECULAR(1.0, 1.0, 1.0, 1.0));
  glMaterial ~face:GL_FRONT ~mode:(Material.GL_SHININESS(50.0));
  
  glLight ~light:(GL_LIGHT 0) ~pname:(Light.GL_POSITION(0.0, 0.0, 1.0, 0.0));

  glEnable (GL_LIGHTING);
  glEnable (GL_LIGHT0);
  glEnable (GL_DEPTH_TEST);
;;

let () =
  ignore(glutInit Sys.argv);
  glutInitDisplayMode [GLUT_DOUBLE; GLUT_RGBA; GLUT_DEPTH];
  glutInitWindowSize 800 600;
  glutInitWindowPosition 100 100;
  ignore(glutCreateWindow ~title:Sys.argv.(0));
  gl_init();
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

