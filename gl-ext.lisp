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
    
  
    
(defun gl-ext:print-shader-errors (void (shader gl:shader))
  (let ((glstatus (cast 0 u32))
	(log (cast (alloc0 1000) (ptr char))))
    (gl:get-shader-info shader gl:compile-status (addrof glstatus))
    (when (eq glstatus gl:false)
      (let ((length :type u32))
	(gl:get-shader-info-log shader 1000 (addrof length) log)
	(print "**** Shader Info Log ****" newline log newline "********" newline)))))

(defun load-shader (gl:shader-program (frag-src (ptr char)) (vert-src (ptr char)))
  (let ((prog (gl:create-program))
	(frag (gl:create-shader gl:fragment-shader))
	(vert (gl:create-shader gl:vertex-shader))
	(frag-src-len (cast (strlen frag-src) u32))
	(vert-src-len (cast (strlen vert-src) u32)))
    (gl:shader-source frag 1 (addrof frag-src) (addrof frag-src-len))
    (gl:shader-source vert 1 (addrof vert-src) (addrof vert-src-len))
    (gl:compile-shader frag)
    (print "**** Fragment Shader ****" newline)
    (gl-ext:print-shader-errors frag)
    (gl:compile-shader vert)
    (print "**** Vertex Shader ****" newline)
    (gl-ext:print-shader-errors vert)
    (gl:attach-shader prog frag)
    (gl:attach-shader prog vert)
    (gl:link-program prog)    
    (let ((glstatus :type u32))
      (gl:get-program-info prog gl:link-status (addrof glstatus))
      (print "Shader status: " glstatus newline))
    (gl:delete-shader frag)
    (gl:delete-shader vert)
    prog))
  
