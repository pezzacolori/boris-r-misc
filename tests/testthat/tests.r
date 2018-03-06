
context('reorder factor levels with orderfactor')

test_that("orderfactor works with numeric and character vetor to specify levels order", {
  x<-factor(c('a','b','c','b'), ordered=F)
  
  expect_that(levels(orderfactor(x, c(3,1,2))), equals(levels(x)[c(3,1,2)]))
  expect_that(levels(orderfactor(x, c('c','a','b'))), equals(levels(x)[c(3,1,2)]))
  expect_that(is.ordered(orderfactor(x,c(3,1,2))), equals(F))
  expect_that(is.ordered(orderfactor(x,c(3,1,2), ordered=T)), equals(T))
  
})




context('extract dependent (dep.vars) and independent variables (indep.vars) from formula')

test_that("dep_vars works for complete and uncomplete formulae", {

  expect_that(dep_vars(y ~ x +b:c), equals('y'))
  expect_that(dep_vars(y+ z ~ x +b:c), equals(c('y','z')))
  expect_that(dep_vars( ~ x +b:c), equals(NA))
  
})


test_that("ind_vars works for complete and uncomplete formulae, and also for interactions", {
  
  expect_that(ind_vars(y ~ x + b + b:c), equals(c('x','b','c')))
  expect_that(ind_vars(y ~ x + b*c), equals(c('x','b','c')))
  expect_that(ind_vars( ~ x + b:c), equals(c('x','b','c')))
  expect_that(ind_vars(y+z ~ x + b*c), equals(c('x','b','c')))
  
  expect_that(ind_vars(y+z ~ x + I(x^2)), equals(c('x','I(x^2)')))
  expect_that(ind_vars(y+z ~ x + I(x^2), T), equals(c('x')))
  
})


context('fill na values in a vector')

test_that("fill_1_na works", {
  
  expect_that(fill_1_na(c(1,NA,3,4,5,NA)), equals(c(1,2,3,4,5,NA)))
  expect_error(fill_1_na(c(1,NA,3,4,5,NA), 'mm'))
  
})

test_that("fill_na works", {
  
  expect_that(fill_na(c(1,2,NA,NA,NA,6,7,8,9,10,NA), maxgap=3, method= 'l'), equals(c(1:10,NA)))
  expect_that(fill_na(c(1,2,NA,NA,NA,6,7,8,9,10,NA), maxgap=3, method= 'p'), equals(c(1,2,2,2,2,6,7,8,9,10,NA)))
  expect_that(fill_na(c(1,2,NA,NA,NA,6,7,8,9,10,NA), maxgap=3, method= 'n'), equals(c(1,2,6,6,6,6,7,8,9,10,NA)))

  expect_that(fill_na(c(1,2,NA,NA,NA,6,7,8,9,10,NA), maxgap=2, method= 'l'), equals(c(1,2,NA,NA,NA,6,7,8,9,10,NA)))
  
})




