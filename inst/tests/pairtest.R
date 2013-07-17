data(for.exercise, package="snpStats")
test.data <- snps.10[,11:20]

## pairtest.R

context("pairtest.R")

## setup

case<-test.data[subject.support$cc==1]
control<-test.data[subject.support$cc==0]
## create a test matrix of phenotypes
total.cc<-sum(nrow(case),nrow(control))
known.pheno<-matrix(as.integer(0),total.cc,1)
known.pheno[1:nrow(case),1]<-1



test_that("pairtest throws error when either case or control param is not a snpMatrix object",{
  expect_that(pairtest(1,control),throws_error())
  expect_that(pairtest(case,1),throws_error())
})

test_that("pairtest throws error when case and control snpMatrix objects have different SNPs",{
  expect_that(pairtest(case[,1:5],control),throws_error())
})


test_that("pairtest return when n.perm=0",{
  p.1df<-pairtest(case,control)
  ## this is probably enough
  expect_equal(names(p.1df),colnames(case))
  ## however we can check that none >1 and <0
  expect_that(unique(p.1df<0),is_false())
  expect_that(unique(p.1df>1),is_false())
})

test_that("pairtest returns when n.perm>0",{
  p.1df<-pairtest(case,control,5)
  expect_that(p.1df,is_a("matrix"))
  expect_equal(nrow(p.1df),ncol(case))
  expect_equal(ncol(p.1df),5)
})


test_that("pairtest throws error when pheno.perm does not match total sample number",{
  expect_that(pairtest(case,control,pheno.perm=known.pheno[-5,]),throws_error())
})

test_that("pairtest pheno.perm parameter returns correct values",{
  p.1df<-pairtest(case,control)
  names(p.1df)<-NULL
  p.1df.pheno<-pairtest(case,control,pheno.perm=known.pheno)
  expect_that(p.1df.pheno,is_a("matrix"))
  expect_identical(p.1df,p.1df.pheno[,1])
})

