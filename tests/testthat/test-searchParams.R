context("searchParams")

test_that("a new instance of a searchParams class returns an empty URI parameterlist",{
  q <- searchParams$new()
  expect_equal(q$toUriParamString(), "")
})

test_that("all properties are set and overridden correctly with add",{
  q <- searchParams$new()

  # Initializing all properties with a single value
  q$add("_text", "(bone OR liver)")
  q$add("_content", "(bone OR liver)")
  q$add("_query", "name")
  q$add("_sort", "-date")
  q$add("_count", 50)
  q$add("_include", "MedicationRequest:patient")
  q$add("_revinclude", "Provenance:target")
  q$add("_summary", "true")
  q$add("_elements", "identifier")
  q$add("_contained", "false")
  q$add("_containedType", "contained")
  q$add("_filter", "given eq \"peter\" and birthdate ge 2014-10-10")
  q$add("name", "Peter")

  # Check if all properties match the initialized values
  expect_equal(q$.__enclos_env__$private$text, "(bone OR liver)")
  expect_equal(q$.__enclos_env__$private$content, "(bone OR liver)")
  expect_equal(q$.__enclos_env__$private$query, "name")
  expect_equal(q$.__enclos_env__$private$sort, "-date")
  expect_equal(q$.__enclos_env__$private$count, 50)
  expect_equal(q$.__enclos_env__$private$includes, "MedicationRequest:patient")
  expect_equal(q$.__enclos_env__$private$revIncludes, "Provenance:target")
  expect_equal(q$.__enclos_env__$private$summaryType, "true")
  expect_equal(q$.__enclos_env__$private$elements, "identifier")
  expect_equal(q$.__enclos_env__$private$contained, "false")
  expect_equal(q$.__enclos_env__$private$containedType, "contained")
  expect_equal(q$.__enclos_env__$private$filter, "given eq \"peter\" and birthdate ge 2014-10-10")
  expect_equal(q$.__enclos_env__$private$parameters, "name=Peter")

  # Check toUriParamString
  expect_equal(q$toUriParamString(), "_query=name&_text=(bone OR liver)&_content=(bone OR liver)&_count=50&_include=MedicationRequest:patient&_revinclude=Provenance:target&_sort=-date&_summary=true&_filter=given eq \"peter\" and birthdate ge 2014-10-10&_contained=false&_containedType=contained&_elements=identifier&name=Peter")


  # Override values
  q$add("_text", "(bone AND liver)")
  q$add("_content", "(bone AND liver)")
  q$add("_query", "names")
  q$add("_count", 20)
  q$add("_summary", "data")
  q$add("_contained", "both")
  q$add("_containedType", "container")
  q$add("_filter", "given eq \"henk\" and birthdate gt 2013-02-02")

  # Add values
  q$add("_sort", "address")
  q$add("_include", "MedicationPrescription:patient")
  q$add("_revinclude", "MedicationPrescription:patient")
  q$add("_elements", "given")
  q$add("address", "Bos en Lommerplein")

  # Check toUriParamString
  expect_equal(q$toUriParamString(), "_query=names&_text=(bone AND liver)&_content=(bone AND liver)&_count=20&_include=MedicationRequest:patient&_include=MedicationPrescription:patient&_revinclude=Provenance:target&_revinclude=MedicationPrescription:patient&_sort=-date,address&_summary=data&_filter=given eq \"henk\" and birthdate gt 2013-02-02&_contained=both&_containedType=container&_elements=identifier,given&name=Peter&address=Bos en Lommerplein")

  # Check if all overridden and added values are correctly set
  expect_equal(q$.__enclos_env__$private$text, "(bone AND liver)")
  expect_equal(q$.__enclos_env__$private$content, "(bone AND liver)")
  expect_equal(q$.__enclos_env__$private$query, "names")
  expect_equal(q$.__enclos_env__$private$count, 20)
  expect_equal(q$.__enclos_env__$private$summaryType, "data")
  expect_equal(q$.__enclos_env__$private$contained, "both")
  expect_equal(q$.__enclos_env__$private$containedType, "container")
  expect_equal(q$.__enclos_env__$private$filter, "given eq \"henk\" and birthdate gt 2013-02-02")

  expect_equal(q$.__enclos_env__$private$sort, c("-date", "address"))
  expect_equal(q$.__enclos_env__$private$includes, c("MedicationRequest:patient", "MedicationPrescription:patient"))
  expect_equal(q$.__enclos_env__$private$revIncludes, c("Provenance:target", "MedicationPrescription:patient"))
  expect_equal(q$.__enclos_env__$private$elements, c("identifier", "given"))
  expect_equal(q$.__enclos_env__$private$parameters, c("name=Peter", "address=Bos en Lommerplein"))
})

test_that("add with _count parameter only accept numbers greater than 0",{
  q <- searchParams$new()

  # numeric input
  q$add("_count", 10)
  expect_equal(q$.__enclos_env__$private$count, 10)
  expect_error(q$add("_count", -10))
  expect_error(q$add("_count", 0))
  expect_equal(q$.__enclos_env__$private$count, 10)
  q$add("_count", 1)
  expect_equal(q$.__enclos_env__$private$count, 1)

  # character input
  q$add("_count", "10")
  expect_equal(q$.__enclos_env__$private$count, 10)
  expect_error(q$add("_count", "-10"))
  expect_error(q$add("_count", "0"))
  expect_error(q$add("_count", "one"))
  expect_equal(q$.__enclos_env__$private$count, 10)
  q$add("_count", "1")
  expect_equal(q$.__enclos_env__$private$count, 1)
})

test_that("comma-separated values are handled correctly for _elements and _sort",{
  q <- searchParams$new()

  # _elements
  q$add("_elements", "given,address,email,family")
  expect_equal(q$.__enclos_env__$private$elements, c("given","address","email","family"))
  q$add("_elements", "gender,deceased")
  expect_equal(q$.__enclos_env__$private$elements, c("given","address","email","family", "gender", "deceased"))

  # _sort
  q$add("_sort", "-given,address,email,-family")
  expect_equal(q$.__enclos_env__$private$sort, c("-given","address","email","-family"))
  q$add("_sort", "-gender,deceased")
  expect_equal(q$.__enclos_env__$private$sort, c("-given","address","email","-family", "-gender", "deceased"))
})

test_that("all valid types for _summary, _contained & _containedType work", {
  q <- searchParams$new()

  # valid _summary
  q$add("_summary", "true")
  expect_equal(q$.__enclos_env__$private$summaryType, "true")
  q$add("_summary", "data")
  expect_equal(q$.__enclos_env__$private$summaryType, "data")
  q$add("_summary", "count")
  expect_equal(q$.__enclos_env__$private$summaryType, "count")
  q$add("_summary", "text")
  expect_equal(q$.__enclos_env__$private$summaryType, "text")
  q$add("_summary", "false")
  expect_equal(q$.__enclos_env__$private$summaryType, "false")

  # valid _contained
  q$add("_contained", "true")
  expect_equal(q$.__enclos_env__$private$contained, "true")
  q$add("_contained", "false")
  expect_equal(q$.__enclos_env__$private$contained, "false")
  q$add("_contained", "both")
  expect_equal(q$.__enclos_env__$private$contained, "both")

  # valid _containedType
  q$add("_containedType", "container")
  expect_equal(q$.__enclos_env__$private$containedType, "container")
  q$add("_containedType", "contained")
  expect_equal(q$.__enclos_env__$private$containedType, "contained")

  # invalid _summary
  expect_error(q$add("_summary", "beer"))
  expect_error(q$add("_summary", "fals"))
  expect_error(q$add("_summary", "trues"))

  # invalid _contained
  expect_error(q$add("_contained", "beer"))
  expect_error(q$add("_contained", "fals"))
  expect_error(q$add("_contained", "trues"))

  # invalid _containedType
  expect_error(q$add("_containedType", "beer"))
  expect_error(q$add("_containedType", "containe"))
  expect_error(q$add("_containedType", "containeds"))
})
