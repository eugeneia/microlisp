# Microlisp

**Description:**

A simple Lisp compiler. Compiles a minimal Lisp dialect ("Microlisp") to
ANSI C. The compiler is written in Common Lisp. The Microlisp dialect is
very simplistic. It features lists, real numbers, symbols, characters and
a macro facility borrowed from Common Lisp. All data is immutable and a
reference counting garbage collector is employed.

I developed this project as my apprenticeship's final practical exam.  I
haven't done much besides bug fixing since then (2012) and due to the
nature of the circumstances some parts of the source code—especially the
compiler—are somewhat rushed. While the inline documentation of the
source code is complete and written in english, the included papers are
written in german. For people trying to get a grasp of Microlisp I
recommend reading the source files in the {test/} and {includes/}
directories.

The codebase is very well suited for educational purposes. The original
goal of Microlisp was to be a dynamic scripting language for embedded
systems. It never was deployed on a real embedded system though.

**Documentation:**

* [Paper on Microlisp](http://mr.gy/software/microlisp/microlisp-architektur.html)
  (in German)
* [User manual](http://mr.gy/software/microlisp/microlisp-bedienungsanleitung.html)
  (in German)

