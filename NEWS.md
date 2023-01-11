# pufr 0.3.0

* `crps_uniqueness` now returns a list of values instead of the average uniqueness. (#1)
* `intra_hd` returns a 2D matrix if the argument supplied is a 3D array.
* Added `%<>%` and `%</>%` operators for hamming distance.

# pufr 0.2.1

* Add `register_parallel` function to register a parallel cluster.

# pufr 0.2.0

* Add automatic detection of the `parallel` package to parallelize the computations.
* Renamed `uniqueness` to `crps_uniqueness`.
* Renamed `entropy_lgl` to `entropy_bits`.
* Add unit test suite.

# pufr 0.1.0

* Added a `NEWS.md` file to track changes to the package.
