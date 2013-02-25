(*
let _ =
   ignore( Glut.init Sys.argv );
   Glut.initDisplayMode ~double_buffer:true ();
   Glut.initWindowSize ~w:500 ~h:500;
   ignore (Glut.createWindow ~title:"Papper Planes");
   let angle t = 10. *. t *. t in
   let render () =
     GlClear.clear [ `color ];
     GlMat.load_identity ();
     GlMat.rotate ~angle: (angle (Sys.time ())) ~z:1. ();
     GlDraw.begins `triangles;
     List.iter GlDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];
     GlDraw.ends ();
     Glut.swapBuffers () in
   GlMat.mode `modelview;
   Glut.displayFunc ~cb:render;
   Glut.idleFunc ~cb:(Some Glut.postRedisplay);
   Glut.mainLoop ()

*)
open GL
open Glut

(* mouse coordinates *)
let xold = ref 0
let yold = ref 0

let b_down = ref false

let angley = ref 0
let anglex = ref 0

let display() =
  glClear [GL_COLOR_BUFFER_BIT];
  glLoadIdentity();
  glRotate ~angle:(float(- !angley)) ~x:1.0 ~y:0.0 ~z:0.0;
  glRotate ~angle:(float(- !anglex)) ~x:0.0 ~y:1.0 ~z:0.0;
  glColor3 ~r:0. ~g:1.0 ~b:0.;
  glutWireCube ~size:1.0;
  glFlush();
  glutSwapBuffers();
;;

(* active mouse motion *)
let motion ~x ~y =
  if !b_down then  (* if the left button is down *)
  begin
 (* change the rotation angles according to the last position
    of the mouse and the new one *)
    anglex := !anglex + (!xold - x);
    angley := !angley + (!yold - y);
    glutPostRedisplay();
  end;
  xold := x;  (* save mouse position *)
  yold := y;
;;

(* mouse button event *)
let mouse ~button ~state ~x ~y =
  match button, state with
  (* if we press the left button *)
  | GLUT_LEFT_BUTTON, GLUT_DOWN ->
      b_down := true;
      xold := x;  (* save mouse position *)
      yold := y;
  (* if we release the left button *)
  | GLUT_LEFT_BUTTON, GLUT_UP ->
      b_down := false;
  | _ -> ()
;;

let keyboard ~key ~x ~y =
  match key with
  | '\027' (* escape key *)
  | 'q' -> exit 0
  | _ -> ()
;;

let () =
  ignore(glutInit Sys.argv);
  glutInitDisplayMode [GLUT_DOUBLE];
  ignore(glutCreateWindow ~title:Sys.argv.(0));
  glutDisplayFunc ~display;
  glutKeyboardFunc ~keyboard;
  glutMouseFunc ~mouse;
  glutMotionFunc ~motion;
  glutMainLoop();
;;

