(load "../Photon/std2.lisp")
(load "truetype.lisp")

(defstruct rect
  (upper-left vec2)
  (size vec2))

(defvar rect-default :type rect)

(memset (cast (addrof rect-default) (ptr void)) 0 (size-of (type rect)))
(load "mesh.lisp")
(load "glfw.lisp")
(glfw:init)
(defvar win (glfw:create-window 400 400 "Flowery!" null null))
(glfw:make-current win)
(load "gl.lisp")
(load "gl-ext.lisp")

(defmacro rect-test ()
  (expr (member rect-default upper-left)))
(macrolet ((s3 (member rect-default size)))
  (print s3 newline))
(print rect-test newline)

(defoverloaded delete)
(defoverloaded push)
(defmacro array-type(type)
  (progn
    (when  (eq null (get-var (expr (delete array (unexpr type)))))
      (print "Defining data structures for " (expr (array (unexpr type))) newline)
      (eval!! (intern (expr (defstruct (array (unexpr type))
		     (data (ptr (unexpr type)))
		     (cnt i64)
		     (name (ptr expr))))))
      (eval!! (intern (expr (defun (delete array (unexpr type)) (void (arr (array (unexpr type))))
		     (dealloc (cast (member arr data) (ptr void)))))))
      (eval!! (intern (expr (overload delete (delete array (unexpr type))))))
      (let ((s (cast 
		(size-of (expr2type type))
		i64)))
	(print s newline)
	(eval!! (intern (expr 
			 (defun (push array (unexpr type)) 
			     ((array (unexpr type)) 
			      (arr (array (unexpr type)))
			      (value (unexpr type)))
			   (progn
			     (let ((cnt (cast (member arr cnt) i64)))
			       (let ((newsize (* (+ cnt 1) (unexpr (number2expr s)))))
			       (setf (member arr data) (cast
			   				(realloc 
			   				 (cast (member arr data) (ptr void))
			   				 (cast newsize u64))
			   				(ptr (unexpr type))))
				 
				 (setf (deref (ptr+ (member arr data) cnt)) value)
				 (incr (member arr cnt) 1)))
			     arr)))))
	(eval!! (intern (expr (overload push (push array (unexpr type))))))
	))
    (expr (array (unexpr type)))))

;(array-type i32)
;; (print (expand (expr (array-type i32))) newline)
;; (print (expand (expr (array-type vec2))) newline)
;; (print (expand (expr (array-type vec3))) newline)
;; (print (expand (expr (array-type i64))) newline)

;(exit 0)
;(exit 0)
(defun tst (void (a (array-type i32)))
  (print (member a cnt)))

(defun struct-name ((ptr expr) (struct-type (ptr type_def)))
  (alias-name struct-type))

(defmacro map (array fcn)
  (let ((type :type (ptr expr)))
    (progn
      (let ((at (type-of array)))
	(let ((array-name (struct-name at)))
	  (setf type (sub-expr.expr array-name 1))))
      (let ((test (get-var (intern (expr (map2 (unexpr type)))))))
	(when (eq null test)
	  (eval! (expr 
		  (defun (map2 (unexpr type)) 
		      (void (a (array-type (unexpr type))) (f (ptr (fcn void (v (unexpr type))))))
		    (range it 0 (member a cnt)
			   (f (deref (ptr+ (member a data) it)))))))))
	  (expr ((map2 (unexpr type)) (unexpr array) (unexpr fcn))))))


(print (expr2type (expr i32)) newline)
(defvar exprtype (type (ptr expr)))
(defmacro make-array (&type t args)
  (let ((argcnt (sub-expr.cnt args)))
    (assert (> argcnt 1))
    (unless (eq null (cast t (ptr void))) 
      (setf t (expr2type  (sub-expr.expr (alias-name t) 1)))
      )
    (let ((type (type-of2 t (sub-expr.expr args 1)))
	  (expr-buf (cast (alloc0 (* argcnt (size-of (type (ptr expr)))))
			  (ptr (ptr expr)))))
      (setf (deref expr-buf) (expr progn))
      (range it 1 (cast argcnt i64)
	     (setf (deref (+ expr-buf it))
		   (expr
		    (setf (deref (+ data (unexpr (number2expr (- it 1)))))
			  (unexpr (sub-expr.expr args (cast it u64)))))))
      (let ((n (intern (sub-expr.expr args 0))))
	(let ((endexpr (make-sub-expr expr-buf (cast argcnt u64)))
	      (s (intern (expr (nameof (unexpr n))))))
	  (def s exprtype (cast n (ptr void)))
	  (let ((e (expr 
		    (let ((arr :type (array-type (unexpr (type2expr type))))
			  (data (cast (alloc 
				       (unexpr 
					(number2expr 
					 (cast (* (sub-expr.cnt args) (size-of type)) i64))))
				      (ptr (unexpr (type2expr type))))))
		      (setf (member arr data) data)
		      (setf (member arr name) (unexpr s))
		      (setf (member arr cnt) (unexpr (number2expr (cast (- argcnt 1) i64))))
		      (unexpr endexpr)
		      arr))))
	    (dealloc (cast expr-buf (ptr void)))
	    e))))))
;(make-array :a1 (vec 1 2) (vec 2 3))
;(map (make-array :a1 (vec 1 2) (vec 3 4) (vec 5 6) (vec 7 8)) (lambda (void (a vec2)) (print a newline)))
(defvar a2 (make-array :a1 (the 1 i16) 2 3))
(setf a2 (make-array :a1 1 2 3 4 5 6 7))
(map a2 (lambda (void (a i16)) (print a newline)))
(setf a2 (push (push (push a2 3) 4) 5))
(setf a2 (push a2 3))
(setf a2 (push a2 4))
(setf a2 (push a2 1000))
(map a2 (lambda (void (a i16)) (print a newline)))
;(exit 0)

;((m1))
;(map vec2)
(defvar f1 (lambda (void (v vec2)) (print v newline)))
;(f1 (vec 1 2))
;(defvar a3 (make-array :asd (vec 1 2) (vec 3 4)))
(map (make-array :vex (vec 1 2) (vec 3 4)) f1)
(map (make-array :vex (vec 1 2) (vec 3 4)) (lambda (void (v vec2)) (print (+ v (vec 3.1 4.1)) newline)))
(map (make-array :vex (vec 1 2) (vec 3 4)) f1)
(map (make-array :vex (vec 1 2) (vec 3 4)) f1)
(map (make-array :vex (vec 1 2) (vec 3 4)) f1)
(map (make-array :vex2 (vec 1 2 3) (vec 1 2 3)) (lambda (void (v vec3)) (print v newline)))

(defstruct (gl:buffer vec3)
  (vbo u32)
  (cnt u32))

(defstruct (gl:buffer vec2)
  (vbo u32)
  (cnt u32))

(defstruct (gl:buffer i32)
  (vbo u32)
  (cnt u32))

(defoverloaded load-vbo)

(defun (load-vbo vec3) ((gl:buffer vec3) (array (array-type vec3)))
  (let ((buf2 :type (gl:buffer vec3)))
    (setf (member buf2 cnt) (cast (member array cnt) u32))
    (let ((size (* (member array cnt) 3 (cast (size-of (type f32)) i64))))
      (let ((data (cast (alloc (cast size u64)) (ptr f32))))
	(range it 0 (member array cnt)
	       (let ((i2 (* it 3))
		     (v (deref (ptr+ (member array data) it))))
		 (setf (deref (+ data i2)) (cast (member v x) f32))
		 (setf (deref (+ data i2 1)) (cast (member v y) f32))
		 (setf (deref (+ data i2 2)) (cast (member v z) f32))))
	(range it 0 (* (member array cnt) 3)
	       (print (deref (+ data it)) newline))
	(setf (member buf2 vbo) (gl:gen-buffer))
	(gl:bind-buffer gl:array-buffer (member buf2 vbo))
	(gl:buffer-data gl:array-buffer (cast size u32) (cast data (ptr void)) gl:static-draw )
	(dealloc (cast data (ptr void)))
      ))
    buf2))

(defun (load-vbo vec2) ((gl:buffer vec2) (array (array-type vec2)))
  (let ((buf2 :type (gl:buffer vec2)))
    (setf (member buf2 cnt) (cast (member array cnt) u32))
    (let ((size (* (member array cnt) 2 (cast (size-of (type f32)) i64))))
      (let ((data (cast (alloc (cast size u64)) (ptr f32))))
	(range it 0 (member array cnt)
	       (let ((i2 (* it 2))
		     (v (deref (ptr+ (member array data) it))))
		 (setf (deref (+ data i2)) (cast (member v x) f32))
		 (setf (deref (+ data i2 1)) (cast (member v y) f32))
		 
		 ))
	(range it 0 (* (member array cnt) 3)
	       (print (deref (+ data it)) newline))

	(setf (member buf2 vbo) (gl:gen-buffer))
	(gl:bind-buffer gl:array-buffer (member buf2 vbo))
	(gl:buffer-data gl:array-buffer (cast size u32) (cast data (ptr void)) gl:static-draw )
	(dealloc (cast data (ptr void)))
      ))
    buf2))

(overload load-vbo (load-vbo vec3))
(overload load-vbo (load-vbo vec2))

(defun (load-index-vbo i32) ((gl:buffer i32) (array (array-type i32)))
  (let ((buf :type (gl:buffer i32)))
    (setf (member buf vbo) (gl:gen-buffer))
    (setf (member buf cnt) (cast (member array cnt) u32))
    (gl:bind-buffer gl:element-array-buffer (member buf vbo))
    (gl:buffer-data gl:element-array-buffer (cast (* (member array cnt) 4) u32) 
		    (cast (member array data) (ptr void)) gl:static-draw )
    buf))

(defoverloaded bind-vbo)

(defun (bind-vbo vec3) (void (buf (gl:buffer vec3)) (index u32))
  (progn
    (gl:enable-vertex-attrib-array index)
    (gl:bind-buffer gl:array-buffer (member buf vbo))
    (gl:vertex-attrib-pointer index 3 gl:float gl:false 0 null)))

(defun (bind-vbo vec2) (void (buf (gl:buffer vec2)) (index u32))
  (progn
    (gl:enable-vertex-attrib-array index)
    (gl:bind-buffer gl:array-buffer (member buf vbo))
    (gl:vertex-attrib-pointer index 2 gl:float gl:false 0 null)))

(overload bind-vbo (bind-vbo vec3))
(overload bind-vbo (bind-vbo vec2))

(defun (bind-index i32) (void (index-buffer (gl:buffer i32)))
  (progn
    (gl:bind-buffer gl:element-array-buffer (member index-buffer vbo))))

(defun (render-elements i32) (void (mode gl:enum) (index-buffer (gl:buffer i32)))
  (progn
    ((bind-index i32) index-buffer)
    (gl:draw-elements mode (member index-buffer cnt) gl:uint null)))

(defvar shader:program (load-shader
			"
uniform vec4 color;
void main(){
  gl_FragColor = color;
}
"

"
#version 330
layout(location=0) in vec3 vertex_position;
uniform mat4 matrix;
void main(){
  gl_Position = matrix * vec4(vertex_position, 1.0);
}
"))

(defvar shader:color (gl:get-uniform-location shader:program "color"))
(defvar shader:matrix (gl:get-uniform-location shader:program "matrix"))

(gl:use-program shader:program)
(print "SHADER > " (cast shader:program i32) newline)
(print "GL ERROR: " (gl:get-error) newline)

(defvar vertexes (make-array :vertex 
			     (vec 0.1 0.1 -0.5) (vec 0.5 0.1 -0.5) (vec 0.5 0.5 -0.5) (vec 0.1 0.5 -0.5)
			     (vec 0.1 0.1 -2) (vec 0.5 0.1 -2) (vec 0.5 0.5 -2) (vec 0.1 0.5 -2)))
;(defvar vertexes (make-array :vertex (vec 0.0 0.0) (vec 0.5 0.0) (vec 0.5 0.5) (vec 0.0 0.5)))
(defvar indexes (make-array :index 
			    (the 0 i32) 1 2 3 
			    1 0 7 6
			    0 3 7 6
			    2 1 5 6
			    7 6 5 4))

(defvar vbo1 (load-vbo vertexes))
(defvar idx1 ((load-index-vbo i32) indexes))
(delete vertexes)

(bind-vbo vbo1 0)
(gl:enable gl:cull-face)
((gl cull-face) gl:front)
(range it 0 1000
       (progn
	 (gl:clear-color 0 0 0 1)
	 (gl:clear gl:color-buffer-bit)
	 (gl:uniform shader:color 0.0 1.0 0.0 1.0);
	 (let ((phase (* (cast it f64) 0.01)))
	   (gl:uniform shader:color (vec (cos phase) (sin phase) (cos (+ 2.0 phase)) 1.0))
	   (print phase newline)
	   (gl:uniform shader:matrix (dot (projection-matrix 1.0 1.0 0.1 5.1 ) 
					  (translation-matrix 
					   (vec (sin phase) (cos phase) 0))))
	   )
	 
	 ((render-elements i32) gl:quads idx1)
	 (glfw:swap-buffers win)
	 (glfw:poll-events)
	 (usleep 10000)))
		      
  
  
