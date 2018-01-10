# map 0.0.00.9201

* Update to account for change in name from __fgeo.utils__ to __fgeo.tool__.

# map 0.0.00.9200

* Rename to __fgeo.map__

* `map_tag()` and `map_quad()` pad quadrat names with "0" up to four digits.

# map 0.0.00.9100

* Refactor all functions `theme_*()` for consistency.
* Refactor all functions `*_header()` for consistency.
* Refactor all functions `map_*()` for consistency (internal and arguments)
* Rewrite help file of all functions `map_*()` for consistency.
* `map_tag()` gains arguments `show_page` and `show_subquad`.
* `map_tag()` gains arguments `bl, br, tr, tl` to change page labels.
* `map_tag()` gains argument `subquad_offset` to change origin-subquadrat to 01.
* New data sets: `top1quad` and `top4quad` avoid fixing wrong names in examples.
* New vignette [Mapping Functions](https://forestgeo.github.io/map/articles/map.html) shows output of all `map_*()` functions.
* New function `map_quad()` maps trees within a quadrat standarized by `DBH`.

RELEVANT IMPROVEMENTS FROM OTHER PACKAGES

* New package [__fgeo.utils__](https://forestgeo.github.io/fgeo/reference/fgeo.utils.html) inlcudes helpers `top()` and `rm_dead_twice()`.
* New package [__fgeo__](https://forestgeo.github.io/fgeo/index.html) makes it easy to load all functions with a single call.

# map 0.0.00.9007

* Add `map_sp()` from __forestr__ to keep all mapping functions in __map__.
* Remove data. Instead use data from __bciex__.

# map 0.0.00.9006

* Fix bug. Now `map_tag()` again plots subquadrat labels.

# map 0.0.00.9005

* Rename to map.

# try 0.0.00.9004

* Use data with variables `QX` and `QY` instead of `x` and `y` (#18).

# try 0.0.00.9003

* Added a `NEWS.md` file to track changes to the package.
* A tree is now dead only if all its stems are dead (#9 https://goo.gl/kp8Qgj)
