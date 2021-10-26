# Forge

Forge is a library for 3D CAD data. The aim is to provide functions for representing 3D data with Functional Representatin (FREP, similar to Signed Distance Functions), and Boundary Representation (BREP, using parametric curves and Clojure maps to store topological information).

Using basic Clojure functions, an internal representation is emitted, which can then be compiled or exported. Currently, this internal representation is a Hiccup-style tree, but the form of data may evolve as the library develops.

Compilation currently targets .scad files, and is a near copy of [scad-clj](https://github.com/farrellm/scad-clj). There is a FreeCAD compilation prototype available that emits a python script and uses FreeCAD to export a STEP file from OpenSCAD code. It's not perfect yet, but it's an indication of the direction I want to take this library.

OpenSCAD is a great tool, but it only exports triangulated 3D files, which may not be suitable for all CAD workflows. STEP export is a nice way to use programmatic CAD tools as an input into commercial CAD packages that can import STEP files.

## Status

Alpha. Changes are very likely to occur as I continue to build and discover the best design for this library.

## Usage

Use the git deps dependency:

```clojure
io.github.adam-james-v/forge {:git/sha "b48fc23674862090d8645fc605afbf3dd01ef05e"}
```
