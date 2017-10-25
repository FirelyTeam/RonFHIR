context("fhirClient")

client <- fhirClient$new("http://vonk.furore.com")

test_that("fhirClient checks only connects to FHIR Servers STU 3",{
  # STU 3
  expect_silent(fhirClient$new("http://vonk.furore.com"))
  expect_silent(fhirClient$new("http://test.fhir.org/r3"))
  expect_silent(fhirClient$new("http://fhirtest.uhn.ca/baseDstu3"))

  # STU 2
  expect_error(fhirClient$new("http://spark.furore.com"))
  expect_error(fhirClient$new("http://fhirtest.uhn.ca/baseDstu2"))

  # STU 1
  expect_error(fhirClient$new("http://fhirtest.uhn.ca/baseDstu1"))

  # Random
  expect_error(fhirClient$new("http://furore.com"))
  expect_error(fhirClient$new("abcdefg"))
})

test_that("Paging",{
  bundle <- client$search("Patient", pageSize = 10)
  expect_true(!is.null(bundle))
  expect_true(nrow(bundle$entry) <= 10)
  firstId <- bundle$entry$resource$id[1]

  bundle <- client$continue(bundle)
  expect_true(!is.null(bundle))
  secondId <- bundle$entry$resource$id[2]
  expect_true(firstId != secondId)
})

test_that("Read",{
  loc <- client$read("Location/1")
  expect_true(!is.null(loc))
  expect_equal(loc$address$city, "Den Burg")
  expect_equal(loc$id, "1")
  expect_true(!is.null(loc$meta$versionId))
})

test_that("Search",{
  peters <- client$search("Patient", c("name=Chalmers", "name=Peter"))
  expect_true(nrow(peters$entry) > 0)
  expect_true(all(peters$entry$resource$resourceType == "Patient"))
})

test_that("SearchById", {
  example <- client$searchById("Patient", "example")
  expect_equal(nrow(example$entry), 1)
  expect_equal(example$entry$resource$resourceType, "Patient")
  expect_equal(example$entry$resource$id, "example")
})
