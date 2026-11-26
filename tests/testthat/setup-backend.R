## most tests were written for the R backend, which was the default one before
## mixvlmc 0.3.0; the tests of the C++ backend specify it explicitly
withr::local_options(mixvlmc.backend = "R", .local_envir = testthat::teardown_env())
