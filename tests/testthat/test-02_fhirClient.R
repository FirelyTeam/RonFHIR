context("fhirClient")
library(httr)

client <- fhirClient$new("https://vonk.fire.ly")

test_that("fhirClient checks only connects to FHIR Servers STU 3",{
  # STU 3
  expect_silent(fhirClient$new("https://vonk.fire.ly"))

  # STU 2
  expect_error(fhirClient$new("http://spark.furore.com"))
  expect_error(fhirClient$new("http://fhirtest.uhn.ca/baseDstu2"))

  # STU 1
  expect_error(fhirClient$new("http://fhirtest.uhn.ca/baseDstu1"))

  # Random
  expect_error(fhirClient$new("http://fire.ly"))
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
  DELETE("https://vonk.fire.ly/Patient/exampleR")
  expect_error(client$read("Patient/exampleR"))
  client$update(readLines("../testfiles/example_patient.json"))
  pat <- client$read("Patient/exampleR")
  expect_true(!is.null(pat))
  expect_equal(pat$id, "exampleR")
  expect_equal(pat$birthDate, "1994-04-26")
})

test_that("Search",{
  ron <- client$search("Patient", c("given:exact=Ron", "family:exact=FHIR"))
  expect_true(nrow(ron$entry) > 0)
  expect_true(all(ron$entry$resource$resourceType == "Patient"))
})

test_that("SearchById", {
  pat <- client$searchById("Patient", "exampleR")
  expect_equal(nrow(pat$entry), 1)
  expect_equal(pat$entry$resource$resourceType, "Patient")
  expect_equal(pat$entry$resource$id, "exampleR")
  expect_equal(pat$entry$resource$birthDate, "1994-04-26")
  DELETE("https://vonk.fire.ly/Patient/exampleR")
})
