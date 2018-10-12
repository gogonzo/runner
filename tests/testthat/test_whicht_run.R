context("Running which")
set.seed(11)
x1 <- sample(c(T,F,NA), 20, replace=T)
x2 <- sample(c(T,F,NA), 20, replace=T)
k <- sample(1:20, 20, replace=T)

test_that("whicht_run first and last",{
  for(i in 1:20)
    expect_equal(
      whicht_run(x1,which = "first")[i] ,
      min( which(x1[1:i]), na.rm=T )
    )

  for(i in 1:20)
    expect_equal(
      whicht_run(x1,which = "last")[i] ,
      max( which(x1[1:i]), na.rm=T )
    )
})


test_that("whicht with na_rm=F", {
  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = F)[i] ,
      as.integer(
        ifelse(
          max(which(x2[1:i])) > max(c(0,which(is.na(x2[1:i]) ))),
          max(which(x2[1:i])), NA
        )
      )
    )

  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = F, which = "first")[i] ,
      as.integer(
        ifelse(
          min(which(x2[1:i])) < min(c(which(is.na(x2[1:i])),Inf)),
          min(which(x2[1:i])), NA
        )
      )
    )
})

test_that("whicht with na_rm=T k=4", {
  idx <- as.integer(1:20)
  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = T, k=4)[i] ,
      ifelse(is.finite(max( idx[idx %in% (pmax(1,i-4+1):i) & x2], na.rm=T)),
             max( idx[idx %in% (pmax(1,i-4+1):i) & x2], na.rm=T),
             NA_integer_)
    )

  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = T, k=4, which="first")[i] ,
      ifelse(is.finite(min( idx[idx %in% (pmax(1,i-4+1):i) & x2], na.rm=T)),
             min( idx[idx %in% (pmax(1,i-4+1):i) & x2], na.rm=T),
             NA_integer_)
    )
})


test_that("whicht with na_rm=F k=4", {
  idx <- 1:20
  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = F, k=4)[i] ,
      ifelse(
        max( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T) >
        max( c(0,idx[ idx %in% (pmax(1,i-3):i) & is.na(x2) ]), na.rm=T),
        max( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T), NA_integer_
      )
    )

  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = F, k=4, which="first")[i] ,
      ifelse(
        min( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T) <
        min( c(Inf,idx[ idx %in% (pmax(1,i-3):i) & is.na(x2)]), na.rm=T),
        min( idx[ idx %in% (pmax(1,i-3):i) & x2], na.rm=T), NA_integer_
      )
    )
})


test_that("whicht with na_rm=T k=k", {
  idx <- as.integer(1:20)
  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = T, k=k)[i] ,
      ifelse(is.finite(max( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T)),
         max( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T),
         NA_integer_)
    )


  for(i in 1:20)
    expect_equal(
      whicht_run(x2, na_rm = T, k=k, which="first")[i] ,
      ifelse(is.finite(min( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T)),
             min( idx[idx %in% (pmax(1,i-k[i]+1):i) & x2], na.rm=T),
             NA_integer_)
    )
})

test_that("whicht with sequential indexes equals non-indexed",{
  x <- sample(c(NA,F,T),10, replace=T)
  k <- sample(1:10,10,replace=T)

  expect_identical(
    whicht_run( x, which="last", idx=1:10),
    whicht_run( x, which="last")
  )
  expect_identical(
    whicht_run( x, which="last", k=4,idx=seq(2,20, by=2) ),
    whicht_run( x, which="last", k=2 )
  )
  expect_identical(
    whicht_run( x, which="last", k=k*2,idx=(1:10)*2 ),
    whicht_run( x, which="last", k=k )
  )

})

test_that("whicht for indexed window",{
  x <- c(NA,F, T, NA, F, F, T, T, NA, T, F , T)
  i <- c(1, 2, 3, 3,  3, 4, 6, 8, 9,  9, 13, 13)
  k <- sample(1:12,12,replace=T)

  expect_identical(
    whicht_run( x, which="last", idx=i, k=3, na_rm=F),
    as.integer(c(NA, NA, 3, NA, NA, NA, 7, 8, NA, 10, NA, 12))
  )
  expect_identical(
    whicht_run( x, which="first", idx=i, k=2, na_rm=F),
    as.integer(c(NA, NA, 3, 3, 3, 3, 7, 8, 8, 8, NA, 12))
  )
  expect_identical(
    whicht_run( x, which="last", idx=i, k=2, na_rm=T),
    as.integer(c(NA, NA, 3, 3, 3, 3, 7, 8, 8, 10, NA, 12))
  )

})

test_that("Error handling in max_run",{
  expect_error(whicht_run(x2, k=c(2,2,2,2,NA)))
  expect_error(whicht_run(x2, k=c(2,2,2,2,2,2)))
})


