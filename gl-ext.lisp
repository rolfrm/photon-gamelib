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

(defun (vec2 array to vec2f array) ((array-type vec2f) (arr (array-type vec2)))
  (let ((out (default array vec2f)))
    (range it 0 (member arr cnt)
	   (let ((v (get arr it)))
	     (setf out (push out (vec (cast (member v x) f32)
				      (cast (member v y) f32))))
	     ))
    out))

(defun (vec3 array to vec3f array) ((array-type vec3f) (arr (array-type vec3)))
  (let ((out (default array vec3f)))
    (range it 0 (member arr cnt)
	   (let ((v (get arr it)))
	     (setf out (push out (vec (cast (member v x) f32)
				      (cast (member v y) f32)
				      (cast (member v z) f32))))
	     ))
    out))

(defun (load-vbo vec3) ((gl:buffer vec3) (array (array-type vec3)))
  (let ((buf2 :type (gl:buffer vec3)))
    (setf (member buf2 cnt) (cast (member array cnt) u32))
    (let ((size (* (member array cnt) 3 (cast (size-of (type f32)) i64))))
      (let ((data ((vec3 array to vec3f array) array)))
	(setf (member buf2 vbo) (gl:gen-buffer))
	(gl:bind-buffer gl:array-buffer (member buf2 vbo))
	(gl:buffer-data gl:array-buffer (cast size u32) (cast data (ptr void)) gl:static-draw )
	(clear data)
      ))
    buf2))

(defun (load-vbo vec2) ((gl:buffer vec2) (array (array-type vec2)))
  (let ((buf2 :type (gl:buffer vec2)))
    (setf (member buf2 cnt) (cast (member array cnt) u32))
    (let ((size (* (member array cnt) 2 (cast (size-of (type f32)) i64))))
      (let ((data ((vec2 array to vec2f array)  array)))
	(setf (member buf2 vbo) (gl:gen-buffer))
	(gl:bind-buffer gl:array-buffer (member buf2 vbo))
	(gl:buffer-data gl:array-buffer (cast size u32) (cast (member data data) (ptr void)) gl:static-draw )
	(clear data)
	))
    buf2))

(defoverloaded reload-vbo)
(defun (reload-vbo vec2) ((gl:buffer vec2) (vbo (gl:buffer vec2)) (array (array-type vec2)))
  (let ((data ((vec2 array to vec2f array) array)))
    (gl:bind-buffer gl:array-buffer (member vbo vbo))
    (gl:buffer-data gl:array-buffer (cast (size data) u32) (cast (member data data) (ptr void)) gl:static-draw)
    (setf (member vbo cnt) (cast (member data cnt) u32))
    (clear data)
    vbo
    ))

(overload load-vbo (load-vbo vec3))
(overload load-vbo (load-vbo vec2))
(overload reload-vbo (reload-vbo vec2))
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
  (gl:bind-buffer gl:element-array-buffer (member index-buffer vbo)))

(defun (render-elements i32) (void (mode gl:enum) (index-buffer (gl:buffer i32)))
  (progn
    ((bind-index i32) index-buffer)
    (gl:draw-elements mode (member index-buffer cnt) gl:uint null)))
