Jumpy
=====

A small game written in haskell that uses lenses in combination with the state monad to create imperative-like code.

Building/running
----------------

If on linux just type `./run`, otherwise, `cabal configure` then `cabal build` will give an executable in `dist/build/Jumpy`.

Todo
----

* Profiling shows `diffTime` is using the most CPU time. Optimize it and minimize its use to once per frame.
* Add images, maybe animations and particle affects.
* A proper start menu.
