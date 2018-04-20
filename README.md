# RonFHIR
R on FHIR is an easy to use wrapper around the 'HL7 FHIR' REST API (STU 3). It provides tools to easily read and search resources on a FHIR server and bring the results into the R environment. R on FHIR is based on the FhirClient of the official '[HL7 FHIR .NET API](https://github.com/ewoutkramer/fhir-net-api)', also made by [Firely](https://fire.ly/).

## Installation
```r
# To download the latest release from CRAN use:
install.packages("RonFHIR")

# Or download the development version from GitHub:
# install.packages("devtools")
devtools::install_github("FirelyTeam/RonFHIR")
```

## Overview
 - Read
 - Search
 - GraphQL
 - Operations
 - Paging mechanism
 - Fluent search calls
 - OAuth 2.0

## Usage
```r
library(RonFHIR)
# Setting up a fhirClient
client <- fhirClient$new("https://vonk.fire.ly/")

# Setting up a fhirClient with OAuth 2.0
client <- fhirClient$new("https://vonk.fire.ly")

client_id <- "id"
client_secret <- "secret"
app_name <- "TestApp"
scopes <- c("patient/*.read")

app <- httr::oauth_app(appname = app_name, client_id, client_secret)
oauth_endpoint <- httr::oauth_endpoint(authorize = paste(client$authUrl, "?aud=", client$endpoint, sep=""), access = client$tokenUrl)

token <- httr::oauth2.0_token(endpoint = oauth_endpoint, app = app, scope = scopes)

client$setToken(token)

# Search
bundle <- client$search("Patient", c("name=Peter", "address-postalcode=3999"))

while(!is.null(bundle)){
  # Do something useful here
  
  # Go to the next page of the bundle using FHIRs paging mechanism
  bundle <- client$continue(bundle)
}

# Searching with a searchParams object
query <- searchParams$new()
query$select(c("name", "birthDate"))$where("given:exact=Peter")$orderBy("family")

peters <- client$searchByQuery(query, "Patient") 
# equivalent: client$search("Patient", c("_elements=name,birthDate","given:exact=Peter", "_sort=family"))

#GraphQL read
client$qraphQL("{id name{given,family}}", "Patient/example")

#GraphQL read
client$qraphQL("{PatientList(name:\"pet\"){name @first @flatten{family,given @first}}}")

# Operations
client$operation("Observation", name = "lastn")
```
