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

(defmacro array (item &rest args)
  (let ((argcnt (sub-expr.cnt args)))
    (assert (> argcnt 0))
    (let ((type (type-of (sub-expr.expr args 0)))
	  (expr-buf (cast (alloc0 (* (+ 1 argcnt) (size-of (type (ptr void)))))
			  (ptr (ptr expr)))))
      (setf (deref expr-buf) (expr progn))
      (range it 0 (cast argcnt i64)
	     (setf (deref (+ expr-buf it 1))
		   (expr
		    (setf (deref (+ data (unexpr (number2expr it)))) 
			  (unexpr (sub-expr.expr args (cast it u64)))))))
      (let ((endexpr (make-sub-expr expr-buf (cast (+ argcnt 1) u64))))
	(let ((e (expr 
		  (progn
		    (defstruct (array (unexpr (type2expr type)))
		      (data (ptr (unexpr (type2expr type))))
		      (cnt i64)
		      (name (ptr symbol))
		      )
		    (let ((arr :type (array (unexpr (type2expr type))))
			  (data (cast (alloc 
				       (unexpr 
					(number2expr 
					 (cast (* (sub-expr.cnt args) (size-of type)) i64))))
				      (ptr (unexpr (type2expr type))))))
		      (setf (member arr data) data)
		      (setf (member arr name) (quote (unexpr  item)))
		      (setf (member arr cnt) (unexpr (number2expr (cast (sub-expr.cnt args) i64))))
		      (unexpr endexpr)
		      arr)))))
	  (dealloc (cast expr-buf (ptr void)))
	  e)))))
(vec 0 0)

(let ((arr (array :vertexes (vec 0 0) (vec 1 1) (vec 2 2) (vec 3 3) (vec 4 4) (vec 5 6))))
  (range i 0 (member arr cnt)
	 (print (deref (+ (cast (member arr data) (ptr vec2)) i)) newline)))

(defvar array-a (array :uvs (vec 0 0) (vec 0.5 0) (vec 0.5 0.5) (vec 0.0 0.5)))
(defvar array-b (array :vertexes (vec 0 0) (vec 1 1) (vec 2 2)))
(defvar index-array (array :index 0 1 2 3))
(defvar simple-array (array :vertexes (cast 1.0 f32) (cast 1.0 f32) (cast 1.0 f32) (cast 1.0 f32)))
(defvar simple-array2 (array :vertexes 0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0))
(defvar simple-array3 (the (array :vertexes 0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0) (array f64)))
(exit 0)

(defun compare-arrays (bool (a (array vec2)) (b (array vec2)))
  (let ((cnt-a (member a cnt)))
    (and (eq cnt-a (member b cnt))
	 (and
	  (eq (member a name) (member b name))
	  (let ((result true))
	    (range it 0 cnt-a
		   (setf result (and result (vec2:eq 
					     (deref (+ (member a data) it))
					     (deref (+ (member b data) it))))))
	    result)))))

(defun (load (array f32)) (u32 (array (array f32)))
  (let ((out (gl:gen-buffer)))
    (gl:bind-buffer gl:array-buffer out)
    (gl:buffer-data gl:array-buffer (cast (* (member array cnt) 4) u32) (cast (member array data) (ptr void)) gl:static-draw)
    out))

(defun (load (array f64)

(print "array: " ((load (array f32)) simple-array) newline)

(print "EQ?" (compare-arrays array-a array-b) newline)
(defvar shader:program (gl:create-program))
(defvar shader:color :type gl:uniform-loc)
(let 
    ((frag-src "
uniform vec4 color;
void main(){
  gl_FragColor = color;
}
")

     (vert-src "
#version 130
in vec2 vertex_position;

void main(){
  gl_Position = vec4(vertex_position,0.0,1.0);
}
")
     (frag (gl:create-shader gl:fragment-shader))
     (vert (gl:create-shader gl:vertex-shader)))
  (let ((frag-src-len (cast (strlen frag-src) u32))
	(vert-src-len (cast (strlen vert-src) u32)))
    (gl:shader-source frag 1 (addrof frag-src) (addrof frag-src-len))
    (gl:shader-source vert 1 (addrof vert-src) (addrof vert-src-len)))
  (gl:compile-shader frag)
  (gl:compile-shader vert)
  (print "**** Fragment Shader ****" newline)
  (gl-ext:print-shader-errors frag)
  (print "**** Vertex Shader ****" newline)
  (gl-ext:print-shader-errors vert)
  (gl:attach-shader shader:program frag)
  (gl:attach-shader shader:program vert)
  (gl:link-program shader:program)
  (setf shader:color 
	(gl:get-uniform-location shader:program "color")))
(gl:use-program shader:program)

(range it 0 1000
       (progn
	 (usleep 10000)
	 (gl:clear-color 1 1 1 1)
	 (gl:clear gl:color-buffer-bit)

	 (glfw:swap-buffers win)
	 (glfw:poll-events)))
		      
  
  
