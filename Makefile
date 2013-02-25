target: ; ocamlopt -I +lablGL lablglut.cmxa lablgl.cmxa PaperPlanes.ml -o PaperPlanes && ./PaperPlanes
sth: ; ocaml -I +glMLite GL.cma Glut.cma PaperPlanes.ml -o PaperPlanes && ./PaperPlanes

