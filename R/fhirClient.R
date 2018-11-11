#' fhirClient
#'
#' Read and search only client in R for FHIR STU 3.
#' Based on \href{https://github.com/ewoutkramer/fhir-net-api}{the official HL7 FHIR .NET API.}
#'
#' @section Usage:
#' \preformatted{
#' client <- fhirClient$new(endpoint, token = NULL)
#'
#' client$read(location, summaryType = NULL, returnType = "parsed")
#' client$search(resourceType, criteria = NULL, includes = NULL, pageSize = NULL, summaryType = NULL, returnType = "parsed")
#' client$searchById(resourceType, id, includes = NULL, summaryType = NULL, returnType = "parsed")
#' client$wholeSystemSearch(criteria = NULL, includes = NULL, pageSize = NULL, summaryType = NULL, returnType = "parsed")
#' client$searchParams(params, resourceType = NULL, returnType = "parsed")
#' client$continue(bundle)
#' 
#' 
#' client$setToken(token)
#' 
#' client$endpoint
#' client$authUrl
#' client$tokenUrl
#' client$registerUrl
#' client$token
#' 
#' print(client)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{client}{A \code{fhirClient} object.}
#'   \item{endpoint}{The URL of the server to connect to.}
#'   \item{token}{An ouath 2.0 Token (httr Token 2.0)}
#'   \item{resourceType}{The type of resource to search for.}
#'   \item{id}{The id of the Resource to search for.}
#'   \item{summaryType}{Whether to include only return a summary of the Resource(s).}
#'   \item{location}{The url of the Resource to fetch. This can be a Resource id url or a version-specific.}
#'   \item{criteria}{The search parameters to filter the Resources on. Each given string is a combined key/value pair (separated by '=').}
#'   \item{includes}{Paths to include in the search.}
#'   \item{pageSize}{Asks server to limit the number of entries per page returned.}
#'   \item{query}{A searchParams object containing the search parameters.}
#'   \item{bundle}{The bundle as received from the last response.}
#'   \item{returnType}{Specify the return type. This can be "parsed", "json" or "xml".}
#' }
#'
#' @section Details:
#' \code{$new()} Creates a new fhirClient using a given endpoint.
#' If the endpoint does not end with a slash (/), it will be added.
#'
#' \code{$read()} Fetches a typed Resource from a FHIR resource endpoint.
#'
#' \code{$search()} Search for Resources of a certain type that match the given criteria.
#'
#' \code{$searchById()} Search for Resources based on a Resource's id.
#'
#' \code{$wholeSystemSearch()} Search for Resources across the whole server that match the given criteria.
#'
#' \code{$searchByQuery()} Search for Resources based on a searchParams object.
#'
#' \code{$continue()} Uses the FHIR paging mechanism to go navigate around a series of paged result Bundles.
#' 
#' \code{$setToken()} Saves an Oauth 2.0 token in a variable.
#' 
#' \code{$endpoint} Returns the endpoint.
#' 
#' \code{$authUrl} Returns the authorization server’s OAuth authorization endpoint.
#' 
#' \code{$tokenUrl} Returns the authorization server’s OAuth token endpoint.
#' 
#' \code{$registerUrl} Returns the endpoint where the client can register.
#' 
#' \code{$token} Returns the initialized token.
#'
#' \code{print(p)} or \code{p$print()} Shows which endpoint is configured.
#'
#' @importFrom R6 R6Class
#' @importFrom httr GET
#' @importFrom httr PUT
#' @importFrom httr POST
#' @importFrom httr DELETE
#' @importFrom httr content
#' @importFrom httr http_error
#' @importFrom httr http_status
#' @importFrom httr config
#' @importFrom httr content_type_json
#' @importFrom httr accept_json
#' @importFrom httr add_headers
#' @importFrom httr headers
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite validate
#' @importFrom utils URLencode
#' @importFrom stringr str_match
#' @name fhirClient
#'
#' @examples
#' \dontrun{
#' # Setting up a fhirClient
#' client <- fhirClient$new("https://vonk.fire.ly")
#' # Read
#' client$read("Patient/example")
#'
#' # Search
#' bundle <- client$search("Patient", c("name=Peter", "address-postalcode=3999"))
#'
#' while(!is.null(bundle)){
#'    # Do something useful
#'    bundle <- client$continue(bundle)
#' }
#' }
#' \dontrun{
#' # Using Oauth 2.0
#' client <- fhirClient$new("https://vonk.fire.ly")
#' 
#' # Retrieving a token
#'
#' client_id <- "id"
#' client_secret <- "secret"
#' app_name <- "TestApp"
#' scopes <- c("patient/*.read")
#' 
#' app <- httr::oauth_app(appname = app_name, client_id, client_secret)
#' oauth_endpoint <- httr::oauth_endpoint(
#'                   authorize = paste(client$authUrl, "?aud=", client$endpoint, sep=""),
#'                   access = client$tokenUrl)
#'   
#' token <- httr::oauth2.0_token(endpoint = oauth_endpoint, app = app, scope = scopes)
#' 
#' # Set a token and read a patient resource
#' client$setToken(token$credentials$access_token)
#' 
#' client$read("Patient/example")
#' 
#' # Token refresh
#' token <- token$refresh()
#' 
#' client$setToken(token$credentials$access_token)
#' 
#' }
#'
NULL

#' @export
fhirClient <- R6Class("fhirClient",
                      public = list(
                        # Initializing the fhirClient
                        initialize = function(endpoint, token = NULL)
                          execInitialize(self, endpoint, token),

                        # Methods
                        read = function(location, summaryType = NULL, returnType = "parsed")
                          execRead(self, location, summaryType, returnType),
                        search = function(resourceType, criteria = NULL, includes = NULL, pageSize = NULL, summaryType = NULL, returnType = "parsed")
                          execSearch(self, resourceType, criteria, includes, pageSize, summaryType, returnType),
                        searchById = function(resourceType, id, includes = NULL, summaryType = NULL, returnType = "parsed")
                          execSearchById(self, resourceType, id, includes, summaryType, returnType),
                        wholeSystemSearch = function(criteria = NULL, includes = NULL, pageSize = NULL, summaryType = NULL, returnType = "parsed")
                          execWholeSystemSearch(self, criteria, includes, pageSize, summaryType, returnType),
                        searchByQuery = function(params, resourceType = NULL, returnType = "parsed")
                          execSearchByQuery(self, params, resourceType, returnType),
                        qraphQL = function(query, location = NULL, returnType = "parsed")
                          execGraphQL(self, query, location, returnType),
                        continue = function(bundle)
                          execContinue(self, bundle),
                        operation = function (resourceType = NULL, id = NULL, name, parameters = NULL, returnType = "parsed") 
                          execOperation(self, resourceType, id, name, parameters, returnType),
                        
                        update = function(resource)
                          execUpdate(self, resource),
                        print = function()
                          execPrint(self),
                        
                        # Authorization method
                        setToken = function(token)
                          execSetToken(self, token),
                        
                        # Public variables
                        endpoint = NULL,
                        token = NULL,
                        tokenUrl = NULL,
                        authUrl = NULL,
                        registerUrl = NULL
                      )
)


execInitialize <- function(self, endpoint, token) {
  if(substr(endpoint, nchar(endpoint), nchar(endpoint)) != "/"){
    endpoint <- paste(endpoint, "/", sep="")
  }

  self$endpoint <- endpoint
  
  url <- paste(self$endpoint, "metadata?_summary=true", sep = "")
  
  payload <- getResource(self, url, "parsed")
  
  tryCatch(payload$resourceType == "CapabilityStatement", error = function(e){stop("Could not connect to endpoint", call. = FALSE)})

  fhirVersion <- substr(payload$fhirVersion, 1, 1)
  if(fhirVersion != "3"){
    stop(paste("R on FHIR is not compatible with", fhirVersion, "only with STU 3"), call. = FALSE)
  }
  
  if("security" %in% names(payload$rest)){
    lapply(payload$rest$security$extension[[1]]$extension, 
           function(x){
             self$authUrl <<- x$valueUri[match("authorize", x$url)]
             self$tokenUrl <<- x$valueUri[match("token", x$url)]
             self$registerUrl <<- x$valueUri[match("register", x$url)]
           }
    )
    if(is.null(token)){
      warning("The endpoint requires authorization.", call. = FALSE)
    }
    else{
      execSetToken(self, token)
    }
  }
}

execRead <- function(self, location, summaryType, returnType){
  url <- toReadURL(self, location, summaryType)
  getResource(self, url, returnType)
}

execGraphQL <- function(self, query, location, returnType){
  url <- toGraphQLURL(self, location, query)
  getResource(self, url, returnType)
}

execSearch <- function(self, resourceType, criteria, includes, pageSize, summaryType, returnType){
  url <- toSearchURL(self, resourceType, criteria, includes, pageSize, summaryType, NULL)
  getResource(self, url, returnType)
}

execSearchById <- function(self, resourceType, id, includes, summaryType, returnType){
  url <- toSearchURL(self, resourceType, paste("_id=", id, sep = ""), includes, NULL, summaryType, NULL)
  getResource(self, url, returnType)
}

execWholeSystemSearch <- function(self, criteria, includes, pageSize, summaryType, returnType){
  url <- toSearchURL(self, NULL, criteria, includes, pageSize, summaryType, NULL)
  getResource(self, url, returnType)
}

execSearchByQuery <- function(self, query, resourceType, returnType){
  if(!("searchParams" %in% class(query))){
    stop("Parameter is not a valid searchParams object", call. = FALSE)
  }
  url <- toSearchURL(self, resourceType, NULL, NULL, NULL, NULL, query)
  getResource(self, url, returnType)
}

execOperation <- function(self, resourceType, id, name, parameters, returnType) 
{
  url <- toOperationURL(self, resourceType, id, name, parameters)
  getResource(self, url, returnType)
}

execContinue <- function(self, bundle)
{
  returnType = "parsed"
  if(is.character(bundle)){
    # temporary and naive solution untill further investigation of validating XML and JSON against Bundle schema
    bundle <- gsub("\\s", "", bundle)
    
    
    next_url <- str_match(bundle, "<link><relationvalue=\"next\"/><urlvalue=\"(.*?)\"")[1,2]
    if(is.na(next_url)){
      next_url = str_match(bundle, "\"relation\":\"next\",\"url\":\"(.*?)\"")[1,2]
      if(is.na(next_url)){
        stop("Input is not recognized as a Bundle", call. = FALSE)
      }
      else{
        returnType = "json"
      }
    }
    else{
      returnType = "xml"
    }
  }
  else{
    tryCatch(bundle$resourceType == "Bundle", error = function(e){stop("Input is not recognized as a Bundle", call. = FALSE)})
    next_url <- bundle$link[bundle$link$relation == "next",]$url
  }
  
  if(length(next_url) == 0)
  {
    return(NULL)
  }
  else
  {
    if(!startsWith(next_url, self$endpoint)){
      next_url <- paste(substr(self$endpoint, 1, nchar(self$endpoint)-1), next_url, sep = "")
    } 
    getResource(self, next_url, returnType)
  }
}

execUpdate <- function(self, resource){
  putResource(self, resource)
}

execPrint <- function(self){
  cat(
    "Endpoint:", self$endpoint, "\n"
  )
  invisible(self)
}

execSetToken <- function(self, token){
  self$token <- paste("Bearer", token)
}
