input <- readLines("Day8/example.txt")

forest <- matrix(as.integer(str_split(input, "", simplify = T)),    # Convert to numeric matrix
                 nrow = length(input))

test <- c(3,3,5,4,9)

testthat::expect_equal(spot_treesFw(test), c(T, F, T, F, T))
testthat::expect_equal(spot_treesRv(test), c(F, F, F, F, T))
testthat::expect_equal(spot_trees(test), c(T, F, T, F, T))
testthat::expect_equal(sum(spot_trees2D(forest)), 21)

testthat::expect_equal(get_viewFw(test), c(1, 1, 2, 1, 0))
testthat::expect_equal(get_viewRv(test), c(0, 1, 2, 1, 4))
testthat::expect_equal(max(spot_trees2D2(forest)), 8)

