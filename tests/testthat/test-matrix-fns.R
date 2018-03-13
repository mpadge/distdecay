context ("matrix functions")

require (testthat)
require (magrittr)

test_that ('data directories', {
               dd <- dd_get_data_dir ()
               data_dir <- file.path ("data", "data", "bikes")
               data_dir <- paste0 (.Platform$file.sep, data_dir, .Platform$file.sep)
               expect_equal (dd, data_dir)
               #system.file ('extdata', 'tripmats.rda', package = 'distdecay') %>%
               #    file.path () %>%
               #    dirname () %>%
               #    dd_set_data_dir ()
})

city <- "bo" # hard-coded in test data

test_that ('cov matrix', {
               cm <- dd_cov (city = city)
               expect_is (cm, "matrix")
               tm <- tripmats [[city]]
               expect_equal (rownames (tm), rownames (cm))
               expect_equal (colnames (tm), colnames (cm))
})

test_that ('MI matrix', {
#               m <- dd_mi (city = city)
#               expect_is (m, "matrix")
#               expect_true (max (m) < 1)
#               tm <- dd_get_tripmat (city = city)
#               expect_equal (rownames (tm), rownames (m))
#               expect_equal (colnames (tm), colnames (m))
})
