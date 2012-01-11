* Allow :or patterns
* Allow :as patterns to alloe the elimination of explicit arguments for functions. One problem is that we have to decide wherther the :as pattern binds to the forced value or to the delayed one.
* Rebind forced symbols to allow action part to access values without forcing (maybe this is solves by :as patterns).
* Better error checking and more tests


