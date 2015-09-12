(load "../Photon/std2.lisp")
(load "truetype.lisp")
(load "array.lisp")

(defstruct rect
  (upper-left vec2)
  (size vec2))

(defvar rect-default :type rect)
(zeroize rect-default)

(load "mesh.lisp")
(load "glfw.lisp")
(glfw:init)
(defvar win (glfw:create-window 800 800 "Flowery!" null null))
(glfw:make-current win)
(load "gl.lisp")
(load "gl-ext.lisp")

(defmacro rect-test ()
  (expr (member rect-default upper-left)))
(macrolet ((s3 (member rect-default size)))
  (print s3 newline))
(print rect-test newline)

(defvar attrs (cast (alloc0 (* 8 4)) (ptr (ptr char))))
(setf (deref attrs) "vertex_position")
(setf (deref (+ attrs 1)) "colors")
(setf (deref (+ attrs 2)) "colors2")

(defvar shader:program (load-shader
			"
uniform vec4 color;
void main(){
  gl_FragColor = color;
}
"

"
#version 130
in vec3 vertex_position;
uniform mat4 matrix;
void main(){
  gl_Position = matrix * vec4(vertex_position, 1.0);
}
" attrs))

(defvar shader:color (gl:get-uniform-location shader:program "color"))
(defvar shader:matrix (gl:get-uniform-location shader:program "matrix"))

(gl:use-program shader:program)
(print "SHADER > " (cast shader:program i32) newline)
(print "GL ERROR: " (gl:get-error) newline)

(defvar vertexes (make-array :vertex 
			     (vec 0 0 -0.5) (vec 0 2 -0.5) (vec 2 0 -0.5) (vec 2 2 -0.5)
			     (vec 0 0 -2) (vec 2 0 -2) (vec 2 2 -2) (vec 0 2 -2)))
;(defvar vertexes (make-array :vertex (vec 0.0 0.0) (vec 0.5 0.0) (vec 0.5 0.5) (vec 0.0 0.5)))
(defvar indexes (make-array :index (the 0 i32) 1 2 3 4 5 6 7))

(defvar vbo1 (load-vbo vertexes))
(defvar idx1 ((load-index-vbo i32) indexes))
(delete vertexes)

(bind-vbo vbo1 0)
(gl:enable gl:cull-face)
((gl cull-face) gl:front)

(defvar g:up (vec 0 1 0))
(defvar g:right (vec 1 0 0))
(defvar g:depth (vec 0 0 1))

(range it 0 10000
       (progn
	 (gl:clear-color 0 0 0 1)
	 (gl:clear gl:color-buffer-bit)
	 (gl:uniform shader:color 0.0 1.0 0.0 1.0);
	 (let ((phase (* (cast it f64) 0.002)))
	   (gl:uniform shader:color (vec (cos phase) (sin phase) (cos (+ 2.0 phase)) 1.0))
	   (gl:uniform shader:matrix (. 
				      (projection-matrix 2.0 2.0 1.0 20.1 ) 
				      (.
				       (translation-matrix (vec 0 0 -6))
				       (. 
					(mat4-rot-x (* 2.0 phase))
					(.
					 (mat4-rot-z (* 2.0 phase))
					 (mat4-rot-y (* 2.0 phase))
					 )))))
	   )
	 
	 ((render-elements i32) gl:points idx1)
	 (glfw:swap-buffers win)
	 (glfw:poll-events)
	 (usleep 10000)))
		      
  
  
