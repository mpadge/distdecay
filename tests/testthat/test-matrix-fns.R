context ("matrix functions")

require (testthat)
require (magrittr)

test_that ('data directories', {
               dd <- dd_get_data_dir ()
               expect_equal (dd, file.path ("data", "data", "bikes"))
               system.file ('extdata', 'tripmats.rda', package = 'distdecay') %>%
                   file.path () %>%
                   dirname () %>%
                   dd_set_data_dir ()
})

city <- "la" # hard-coded in test data

test_that ('cov matrix', {
               m <- dd_cov (city = city)
               expect_is (m, "matrix")
               tm <- dd_get_tripmat (city = city)
               expect_equal (rownames (tm), rownames (m))
               expect_equal (colnames (tm), colnames (m))
})

test_that ('MI matrix', {
               m <- dd_mi (city = city)
               expect_is (m, "matrix")
               expect_true (max (m) < 1)
               tm <- dd_get_tripmat (city = city)
               expect_equal (rownames (tm), rownames (m))
               expect_equal (colnames (tm), colnames (m))
})
