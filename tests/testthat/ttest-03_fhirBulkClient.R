# named ttest on purpose, rename to test to run (private use only)
context("fhirBulkClient")
library(openssl)
library(jose)
bulkclient <- fhirBulkClient$new("https://bulk-data.smarthealthit.org/eyJlcnIiOiIiLCJwYWdlIjoxMDAwMCwiZHVyIjoxMCwidGx0IjoxNSwibSI6MX0/fhir", tokenURL = "https://bulk-data.smarthealthit.org/auth/token")

test_that("Token retrieval",{
  privatekey <- openssl::read_key("../testfiles/example_private_key.pem")
  
  claim <- jose::jwt_claim(iss = "https://github.com/FirelyTeam/RonFHIR",
                           sub = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJwdWJfa2V5IjoiTFMwdExTMUNSVWRKVGlCUVZVSk1TVU1nUzBWWkxTMHRMUzBLVFVsSlFrbHFRVTVDWjJ0eGFHdHBSemwzTUVKQlVVVkdRVUZQUTBGUk9FRk5TVWxDUTJkTFEwRlJSVUU1VkdkNGRFbFViSFpZT1RoRGF6Z3ZiU3M0VndvNE9ESlRRM0Z6Y0ZkT2RHbHJjelpRVEZjeFRGRTNSbXgzYzJnM2RraFhiMWxSY25Bek4yMU1TakZXVldOMGFrUTJMMnhFY2twRGFIUXpURzFoZG10WUNrY3JhRVYzZVRWTVRGaGFlRlZUT0RGRU0xSjZVbWRJUkV4b2JpOXJjamhhWkU1Q1VuZDZja0Z3ZUd4cVpqWjRhR3BqVEVWRVVISllUSEpzU0d0RU5rNEtTVTFSYVVJMU9UWjZTamN3V0VKbU5UaG5iR1p3YUhGVmNtSjNMMGR2ZW01NlpXa3liRGhyVFVVMFRUZHFVR054Wm5aVFJ6SkZSREZKV0dsNWNraFJad3BPTDBsSWRuZFNjblZrVGpWek9TOUVSR3R2WVRkMlUyTXlORFpaVlZWVWIxbE5Oa05yYzBSd1IwUnhiRTVQYkRGelFVMW1SVXhSUm1NcldUWTBXSHBVQ2xKeWNGSnNTMEZrZVhOWWNYRjVNak5qT1hkRVpHUnFTMFJ4YzFWYVNtcGhaRmhwWXpNMk9VdzJkSEUyWkhSak4xVkNPRXRrTDBKcE1sZFNSell5VFhRS1ZGRkpSRUZSUVVJS0xTMHRMUzFGVGtRZ1VGVkNURWxESUV0RldTMHRMUzB0Q2c9PSIsImlzcyI6Imh0dHBzOi8vZ2l0aHViLmNvbS9GaXJlbHlUZWFtL1JvbkZISVIiLCJhY2Nlc3NUb2tlbnNFeHBpcmVJbiI6MTUsImlhdCI6MTUzMjYwODA2Mn0.d56_EkbxdApwAF2dLPOBicE_xi3Ju9e87GCRO2BjXAM",
                           aud = "https://bulk-data.smarthealthit.org/auth/token",
                           exp = as.integer(as.POSIXct( Sys.time() + 900)), # expiration date as epoch (5 minutes) 
                           jti = charToRaw(as.character(runif(1, 0.5, 100000000000)))) # 'random' number
  
  jwt <- jose::jwt_encode_sig(claim, privatekey)
  
  scopes <- c("system/*.read", "system/CommunicationRequest.write")
  
  token <- bulkclient$retrieveToken(jwt, scopes)
  
  expect_true(!is.null(token$access_token))
})

test_that("groupExport",{
  bulkclient$groupExport(3)
  while(TRUE && bulkclient$getBulkStatus()$progress != "100%")
  {
    s = system.time(Sys.sleep(runif(1, min = 0, max = 0.8)))
    Sys.sleep(1 - s[3])
  }
  data <- bulkclient$downloadBulk(1)
  expect_true(!is.null(data$Patient))
  expect_true(!is.null(data$CarePlan))
  expect_true(!is.null(data$MedicationRequest))
  expect_true(!is.null(data$Observation))
})

test_that("patientExport",{
  bulkclient$patientExport(c("start=2018-06-26"))
  while(TRUE && bulkclient$getBulkStatus()$progress != "100%")
  {
    s = system.time(Sys.sleep(runif(1, min = 0, max = 0.8)))
    Sys.sleep(1 - s[3])
  }
  data <- bulkclient$downloadBulk(1)
  expect_true(!is.null(data$Patient))
  expect_true(!is.null(data$CarePlan))
  expect_true(!is.null(data$MedicationRequest))
  expect_true(!is.null(data$Observation))
})

test_that("Deleting Bulk Request",{
  bulkclient$groupExport(3)
  bulkclient$deleteBulkRequest(1)
  expect_error(bulkclient$getBulkStatus())
})