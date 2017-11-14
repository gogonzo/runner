context("WhichTrue max")
set.seed(11)
x1 <- c(TRUE,  TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE, FALSE, FALSE)
x2 <- c(TRUE, TRUE, NA, NA, TRUE, NA, FALSE, TRUE, TRUE, NA, TRUE, TRUE, NA, NA, TRUE)
k <- c(6, 5, 4, 2, 5, 3, 7, 4, 4, 1, 5, 4, 1, 2, 4)

test_that("whicht_run first and last",{
  for(i in 1:15)
    expect_equal(
      whicht_run(x1,which = "first")[i] ,
      min( which(x1[1:i]), na.rm=T )
    )

  for(i in 1:15)
    expect_equal(
      whicht_run(x1,which = "last")[i] ,
      max( which(x1[1:i]), na.rm=T )
    )
})


test_that("max_run with na_rm=F", {
  for(i in 1:15)
    expect_equal(
      whicht_run(x2, na_rm = F)[i] ,
      as.integer(ifelse(any(is.na(x2[1:i])),NA,max( which(x2[1:i]), na.rm=F )))
    )

  for(i in 1:15)
    expect_equal(
      whicht_run(x2, na_rm = F, which = "first" )[i] ,
      min( which(x2[1:i]), na.rm=F )
    )
})



test_that("max_run with na_rm=T k=4", {
  expect_equal(
    whicht_run(x2, na_rm = T,which = "first",k=4 ) ,
    as.integer(c(1, 1, 1, 1, 2, 5, 5, 5, 8, 8, 8, 9, 11, 11, 12))
  )

  expect_equal(
    whicht_run(x2, na_rm = T,which = "last",k=4 ) ,
    as.integer(c(1, 2, 2, 2, 5, 5, 5, 8, 9, 9, 11, 12, 12,12, 15))
  )
})

test_that("max_run with na_rm=F k=4", {
  expect_equal(
    whicht_run(x2, k=k, na_rm = T,which="last" ) ,
    as.integer(c( 1, 2, 2, NA, 5, 5, 5, 8, 9, NA, 11, 12, NA,NA, 15 ))
  )
})

test_that("max_run pads NA's", {
  expect_identical(
    whicht_run( x2, na_pad=T, k=3, which="first" ),
    as.integer(c( NA, NA, 1, 2, 5, 5, 5, 8, 8, 8, 9,11,11,12,15))
  )
})

test_that("varying window", {
  expect_equal(
    whicht_run(x1,k=k, which="first"),
    c(1,  1,  1,  4,  1,  4,  1,  5,  7, 10,  7, 10, NA, NA, 12)
  )

  expect_equal(
    whicht_run(x1,k=k, which="last"),
    c(1,  2,  2,  4,  5,  5,  7,  8,  8, 10,  11, 12, NA, NA, 12)
  )

})

test_that("Error handling in max_run",{
  expect_error(whicht_run(x2, k=c(2,2,2,2,NA)))
  expect_error(whicht_run(x2, k=c(2,2,2,2,2,2)))
})


