# pufr 0.5.1

* General fixes
* Improved the test suite. This test suite will be used to check the proper behaviour of all the functions and will be used as the reference for the Python version.
* Store names of the crps (devices, challenges and samples) when computing metrics

# pufr 0.5.0

* Make `rbits` create vector, matrix and array, for convenience purposes.
* Added `pufmetrics` class. Allows for automatic plotting and exploration of all metrics.
* Added `compare_pairwise` function to perform functions on all pairs of devices.
* Removed parallel code. If needed, parallelization should be implemented by the user.
* Removed `crps_weight` function in favor of `uniformity` and `bitaliasing`

# pufr 0.4.0

* Renamed `crps_uniqueness` to `uniqueness`.
* Add `entropy_shannon`, `entropy_bits` and `entropy_p`.
* Add `compare_pairwise` to compare a matrix by pairs of rows.
* Make `intra_hd` accept a vector, a matrix and an array.
* Add `reliability` to compute the complementary of `intra_hd`.

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
