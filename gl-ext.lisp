(defun gl-uniform-vec2 (void (location gl:uniform-loc) (v2 vec2))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32)))
(defun gl-uniform-vec3 (void (location gl:uniform-loc) (v2 vec3))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32) 
	      (cast (member v2 z) f32)))

(defun gl-uniform-vec4 (void (location gl:uniform-loc) (v2 vec4))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32) 
	      (cast (member v2 z) f32) (cast (member v2 w) f32)))

(defun (gl:uniform mat4) (void (location gl:uniform-loc) (m mat4))
  (let ((matf (mat4-to-mat4f m)))
    (gl:uniform-mat4-fv location 1 true (cast (addrof matf) (ptr f32)))
    ))

(overload gl:uniform gl-uniform-vec2)
(overload gl:uniform gl-uniform-vec3)
(overload gl:uniform gl-uniform-vec4)
(overload gl:uniform (gl:uniform mat4))

(defun gl:gen-buffer (u32)
    (let (( vbo :type u32))
      (gl:gen-buffers 1 (addrof vbo))
      vbo))
  
(defstruct gl-mesh 
  (vbo u32) ; vertex buffer object
  (ibo u32) ; index buffer object (triangles)
  (dims i32)
  (cnt i32)
  (kind gl:enum)) ; number of indexes

(defun new-gl-mesh (gl-mesh)
  (let ((out :type gl-mesh))
    (setf (member out vbo) -1)
    (setf (member out dims) 0)
    (setf (member out cnt) 0)
    out))
    

;; (defun load-mesh (gl-mesh (_mesh mesh))
;;   (let ((msh (new-gl-mesh))
;; 	(size (* (member _mesh dims)
;; 		 (member _mesh cnt)
;; 		 4)))
;;     (gl:gen-buffers 2 (addrof (member msh vbo))) ;vbo and ibo
;;     (gl:bind-buffer gl:array-buffer (member msh vbo))
;;     (gl:buffer-data gl:array-buffer (cast (member _mesh vertexes) (ptr void)))))

(type (alias u32 gl:error))

(defstruct gl:error-info
  (id gl:error)
  (error-info (ptr expr)))

(defvar gl:shader-program-error (cast 2 gl:error))
(defvar gl:shader-error (cast 1 gl:error))
(defvar gl:no-error (cast 0 gl:error))

(defvar gl:error-list (cast null (ptr gl:error-info)))
(defvar gl:error-list-cnt 0)
(defmacro gl:deferror (name value error-info)
  (let((error-instance :type gl:error-info))
    (setf (member error-instance id) (cast (expr2number value) gl:error))
    (setf (member error-instance error-info) error-info)
    (add-to-list+ gl:error-list gl:error-list-cnt error-instance)
    (expr 
     (defvar (unexpr name) (cast (unexpr value) gl:error)))))


(defun gl:get-error-message ((ptr expr) (id gl:error))
  (let ((o :type gl:error-info))
    (range it 0 gl:error-list-cnt
	   (let ((ins (deref (+ gl:error-list it))))
	     (when (eq (member ins id) id)
	       (setf o ins)
	       (setf it gl:error-list-cnt))))
    (member o error-info)))
     
    

(defun gl-ext:print-shader-errors (gl:error (shader gl:shader))
  (let ((glstatus (cast 0 u32))
	(log (cast (alloc0 1000) (ptr char))))
    (gl:get-shader-info shader gl:compile-status (addrof glstatus))
    (if (eq glstatus gl:false)
	(progn
	  (let ((length :type u32))
	    (gl:get-shader-info-log shader 1000 (addrof length) log)
	    (print "**** Shader Info Log ****" newline log newline "********" newline))
	  gl:shader-error)
	gl:no-error)))

(defun load-shader (gl:shader-program (frag-src (ptr char)) (vert-src (ptr char)) (attributes (ptr (ptr char))))
  (let ((prog (gl:create-program))
	(frag (gl:create-shader gl:fragment-shader))
	(vert (gl:create-shader gl:vertex-shader))
	(frag-src-len (cast (strlen frag-src) u32))
	(vert-src-len (cast (strlen vert-src) u32)))
    (gl:shader-source frag 1 (addrof frag-src) (addrof frag-src-len))
    (gl:shader-source vert 1 (addrof vert-src) (addrof vert-src-len))
    (gl:compile-shader frag)
    (print "**** Fragment Shader ****" newline)
    (assert (eq (gl-ext:print-shader-errors frag) gl:no-error))
      
    (gl:compile-shader vert)
    (print "**** Vertex Shader ****" newline)
    (assert (eq (gl-ext:print-shader-errors vert) gl:no-error))
    (gl:attach-shader prog frag)
    (gl:attach-shader prog vert)
    (when (not (eq (cast attributes (ptr void)) null))
      (let ((it (cast 0 i32)))
	(while! (not (eq (cast null (ptr char)) (deref attributes)))
		(progn
		  ;(print it " " (deref attributes) newline)
		  (gl:bind-attrib-location prog it (deref attributes))
		  (incr attributes 1)
		  (incr it 1)))))
    (gl:link-program prog)    
    (let ((glstatus :type u32))
      (gl:get-program-info prog gl:link-status (addrof glstatus))
      (print "Shader status: " glstatus newline))
    (gl:delete-shader frag)
    (gl:delete-shader vert)
    prog))
  
