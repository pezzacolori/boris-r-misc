
context('reorder factor levels with orderfactor')

test_that("orderfactor works with numeric and character vetor to specify levels order", {
  x<-factor(c('a','b','c','b'), ordered=F)
  
  expect_that(levels(orderfactor(x, c(3,1,2))), equals(levels(x)[c(3,1,2)]))
  expect_that(levels(orderfactor(x, c('c','a','b'))), equals(levels(x)[c(3,1,2)]))
  expect_that(is.ordered(orderfactor(x,c(3,1,2))), equals(F))
  expect_that(is.ordered(orderfactor(x,c(3,1,2), ordered=T)), equals(T))
  
})




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
  
  expect_that(ind.vars(y+z ~ x + I(x^2)), equals(c('x','I(x^2)')))
  expect_that(ind.vars(y+z ~ x + I(x^2), T), equals(c('x')))
  
})


context('fill na values in a vector')

test_that("fill.na works", {
  
  expect_that(fill.na(c(1,2,NA,NA,NA,6,7,8,9,10), maxgap=3, method= 'l'), equals(1:10))
  expect_that(fill.na(c(1,2,NA,NA,NA,6,7,8,9,10), maxgap=3, method= 'p'), equals(c(1,2,2,2,2,6,7,8,9,10)))
  expect_that(fill.na(c(1,2,NA,NA,NA,6,7,8,9,10), maxgap=3, method= 'n'), equals(c(1,2,6,6,6,6,7,8,9,10)))

  expect_that(fill.na(c(1,2,NA,NA,NA,6,7,8,9,10), maxgap=2, method= 'l'), equals(c(1,2,NA,NA,NA,6,7,8,9,10)))
  
})




