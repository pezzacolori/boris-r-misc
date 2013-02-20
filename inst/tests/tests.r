
context('extract dependent (dep.vars) and independent variables (indep.vars) from formula')

test_that("dep.vars works for complete and uncomplete formulae", {

  expect_that(dep.vars(y ~ x +b:c), equals('y'))
  expect_that(dep.vars(y+ z ~ x +b:c), equals(c('y','z')))
  expect_that(dep.vars( ~ x +b:c), equals(NA))
  
})


test_that("ind.vars works for complete and uncomplete formulae, and also for interactions", {
  
  expect_that(ind.vars(y ~ x + b + b:c), equals(c('x','b','c')))
  expect_that(ind.vars(y ~ x + b*c), equals(c('x','b','c')))
  expect_that(ind.vars( ~ x + b:c), equals(c('x','b','c')))
  expect_that(ind.vars(y+z ~ x + b*c), equals(c('x','b','c')))
  
})