Photon Game Library
-------------------

The purpose of this library is to demonstrate the use of Photon as a game programming language. It will consist of a series of abstractions, making it easy and flexible to create high performance game and GUI applications in Photon.

1. The base-level API consists of simple ffi-bindings to libraries like OpenGL and GLFW.
2. Higher 



For the higher level of abstraction I would like something like this:

API Example
-----------
```lisp

(defvar square
  (mesh
    (array :vertexes (verts (vec 0 0) (vec 1 0) (vec 1 1) (vec 0 1)))
    (array :uvs (vec 0 0) (vec 1 0) (vec 1 1) (vec 0 1))
    (array :index 0 1 2 3 4)))

(defvar tex1 (load-texture "tex1.png"))
(defvar m1 (model square tex1))
(defvar position (vec 10 10))
(projection (make-camera (vec 10 10) 0.0 (vec 10 10)) 
  (translate position
    (render m1)))

;; This is the same as 

(scale (vec 0.1 0.1)
  (translate (vec -10 -10)
    (translate (10 10)
      (render m1))))
```

So this looks a bit more declarative than the pure opengl stuff i have been using previously, but I think it will make the code much simpler.

For performance, it should be possible to cache some of the stuff.
```lisp
(render-cache square)
```
This could generate VBOs for the mesh, promising that the content will not change between uses.
