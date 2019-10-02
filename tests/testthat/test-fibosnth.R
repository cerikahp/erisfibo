test_that("checking nth is ok", {
  expect_error(nfibos(-1))
  expect_error(nfibos("hola"))
  expect_error(fibosfrom(1,-2))
  expect_error(fibosfrom("hola"))
})

test_that("is the right number", {
  expect_equal(nfibos(0),0)
  expect_equal(nfibos(1),c(0,1))
  expect_equal(nfibos(2),c(0,1,1))
  expect_equal(nfibos(4),c(0,1,1,2,3))
  expect_equal(fibosfrom(0,4),c(0,1,1,2,3))
  expect_equal(fibosfrom(0,4),c(0,1,1,2,3))
  expect_equal(fibosfrom(1,4),c(1,1,2,3))
})
