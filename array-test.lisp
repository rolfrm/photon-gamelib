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
(exit 0)

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
(exit 0)

;(defmacro generic (&type t body)
;  (let ((name (sub-expr.expr body 0)))
;  (let ((gen-name (sub-expr.expr name 0)))
      

;generic function
(generic (map a) (void (seq (array a)) (f (fcn void (item a))))
  (range it 0 (member seq cnt)
	 (f (deref (ptr+ seq it)))))

((map vec2) (array (vec 0 0) (vec 1 1)) (lambda (void (item vec2)) (print item newline)))
(defun (setf (ptr i32)) (i32 (p (ptr 32)) (value i32))
   (setf (deref p) value))
   
;(defmacro genfun (args body)

(defun fcn1 (void (a (array-type i32)))
  (print (member a cnt) newline))


(let ((arr (array :vertexes (vec 0 0) (vec 1 1) (vec 2 2) (vec 3 3) (vec 4 4) (vec 5 6))))
  (range i 0 (member arr cnt)
	 (print (deref (+ (cast (member arr data) (ptr vec2)) i)) newline)))

(defvar array-a (array :uvs (vec 0 0) (vec 0.5 0) (vec 0.5 0.5) (vec 0.0 0.5)))
(defvar array-b (array :vertexes (vec 0 0) (vec 1 1) (vec 2 2)))
(defvar index-array (array :index 0 1 2 3))
(defvar simple-array (array :vertexes (cast 1.0 f32) (cast 1.0 f32) (cast 1.0 f32) (cast 1.0 f32)))
(defvar simple-array2 (array :vertexes 0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0))
(defvar simple-array3 (the (array :vertexes 0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0) (array f64)))
;(print ">>> " (cast libc i64) " " (cast (load-symbol libc "malloc") i64) newline)
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

;(defun (load (array f64)

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
		      
  
  
