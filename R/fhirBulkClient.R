#' fhirBulkClient
#'
#' Bulk data client in R for FHIR STU 3.
#'
#' @section Usage:
#' \preformatted{
#' bulkclient <- fhirBulkClient$new(endpoint, tokenURL = NULL, token = NULL)
#'
#' bulkclient$patientExport(criteria = NULL)
#' bulkclient$groupExport(groupId, criteria = NULL)
#' bulkclient$wholeSystemExport(criteria = NULL)
#' bulkclient$getBulkStatus()
#' bulkclient$downloadBulk(requestNumber, returnType = "parsed" ,deleteFromQueue = TRUE)
#' bulkclient$deleteBulkRequest(requestNumber)
#' bulkclient$retrieveToken(jwt, scopes, tokenURL = NULL)
#' bulkclient$setToken(token)
#' 
#' print(bulkclient)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{bulkclient}{A \code{fhirBulkClient} object.}
#'   \item{endpoint}{The URL of the server to connect to.}
#'   \item{tokenURL}{Authorization server’s endpoint.}
#'   \item{token}{Acces token.}
#'   \item{criteria}{The search parameters to filter the Resources on. Each given string is a combined key/value pair (separated by '=').}
#'   \item{groupId}{Id of the Group resource.}
#'   \item{requestNumber}{Number of the request in the queue.}
#'   \item{returnType}{Specify the return type. This can be "parsed" or "ndjson".}
#'   \item{deleteFromQueue}{If the request needs to be deleted from the queue after it's been downloaded.}
#'   \item{jwt}{JSON Web Token signed with the app's private key (RSA SHA-256).}
#'   \item{scopes}{Desired scope(s).}
#' }
#'
#' @section Details:
#' \code{$new()} Creates a new fhirBulkClient using a given endpoint.
#' If the endpoint does not end with a slash (/), it will be added.
#'
#' \code{$patientExport()} Request all data on all patients. Possible to filter the results with the _outputFormat, _since and _type parameters. The request will be added to the queue.
#'
#' \code{$groupExport()} Request all data of a patientgroup. Possible to filter the results with the _outputFormat, _since and _type parameters. The request will be added to the queue.
#'
#' \code{$wholeSystemExport()} Request all data. Possible to filter the results with the _outputFormat, _since and _type parameters. The request will be added to the queue.
#'
#' \code{$getBulkStatus()} Update and return the queue to see the progress of your requests.
#'
#' \code{$downloadBulk()} Download a request from the queue.
#'
#' \code{$deleteBulkRequest()} Cancel a request from the queue.
#' 
#' \code{$retrieveToken()} Retrieve a token from the authentication server.
#' 
#' \code{$setToken} Set a token.
#'
#' \code{print(p)} or \code{p$print()} Shows which endpoint is configured.
#'
#' @name fhirBulkClient
#'
#' @examples
#' \dontrun{
#' # Read your private key
#' privatekey <- openssl::read_key("PrivateKey.pem")
#' 
#' # Create your claim
#' claim <- jose::jwt_claim(iss = "ServiceURL",
#'                          sub = "ClientID",
#'                          aud = "TokenURL",
#'                          
#' 			                    # expiration date as epoch (5 minutes)
#'                          exp = as.integer(as.POSIXct( Sys.time() + 300)), 
#'                          
#'    			                # 'random' number
#'                          jti = charToRaw(as.character(runif(1, 0.5, 100000000000)))) 
#' 
#' # Sign your claim with your private key
#' jwt <- jose::jwt_encode_sig(claim, privatekey)
#' 
#' # Define your scope(s)
#' scopes <- c("system/*.read", "system/CommunicationRequest.write")
#' 
#' # Create a new fhirBulkClient
#' bulkclient <- fhirBulkClient$new("FHIRBulkServerURL", tokenURL = "TokenURL")
#' 
#' # Retrieve your token
#' token <- bulkclient$retrieveToken(jwt, scopes)
#' 
#' # Set your token
#' bulkclient$setToken(token$access_token)
#' 
#' # Request a download for Patient Cohort 3
#' bulkclient$groupExport(3)
#' 
#' # Request the progress of the requests
#' bulkclient$getBulkStatus()
#' 
#' # When the downloads a available, download the bulkdata
#' patient_cohort_3 <- bulkclient$downloadBulk(1)
#' 
#' View(patient_cohort_3)
#' }
#'
NULL

#' @export
fhirBulkClient <- R6Class("fhirBulkClient",
                      public = list(
                        # Initializing the fhirBulkClient
                        initialize = function(endpoint, tokenURL = NULL, token = NULL)
                          execInitializeBulk(self, endpoint, tokenURL, token),
                        
                        # Bulk data
                        patientExport = function(criteria = NULL)
                          execPatientExport(self, criteria),
                        
                        groupExport = function(groupId, criteria = NULL)
                          execGroupExport(self, groupId, criteria),
                        
                        wholeSystemExport = function(criteria = NULL)
                          execWholeSystemExport(self, criteria),
                        
                        getBulkStatus = function()
                          execGetBulkStatus(self),
                        
                        downloadBulk = function(requestNumber, returnType = "parsed" ,deleteFromQueue = TRUE)
                          execDownloadBulk(self, requestNumber, returnType, deleteFromQueue),
                        
                        deleteBulkRequest = function(requestNumber)
                          execDeleteBulkRequest(self, requestNumber),
                        
                        print = function()
                          execPrint(self),
                        
                        # Authorization method
                        retrieveToken = function(jwt, scopes, tokenURL = NULL)
                          execRetrieveToken(self, jwt, scopes, tokenURL),
                        
                        setToken = function(token)
                          execSetToken(self, token),
                        
                        # Public variables
                        endpoint = NULL,
                        token = NULL,
                        tokenURL = NULL, # FHIR Bulk Servers don't have a Capability Statement to extract this...
                        queue = data.frame(requestUrl = character(),
                                           statusUrl = character(),
                                           progress=character(),
                                           stringsAsFactors=FALSE)
                      )
)

execInitializeBulk <- function(self, endpoint, tokenURL, token){
  if(substr(endpoint, nchar(endpoint), nchar(endpoint)) != "/"){
    endpoint <- paste(endpoint, "/", sep="")
  }
  
  self$endpoint <- endpoint
  
  if(!is.null(tokenURL)){
    self$tokenURL <- tokenURL
  }
  
  if(!is.null(token)){
    execSetToken(self, token)
  }
}

execPatientExport <- function(self, criteria){
  url <- toExportUrl(self, "Patient", NULL, criteria)
  addToQueue(self, url)
}

execGroupExport <- function(self, groupId, criteria){
  url <- toExportUrl(self, NULL, groupId, criteria)
  addToQueue(self, url)
}

execWholeSystemExport <- function(self, criteria){
  url <- toExportUrl(self, NULL, NULL, criteria)
  addToQueue(self, url)
}



execGetBulkStatus <- function(self){
  if(nrow(self$queue) == 0){
    stop("There are no downloads in the queue", call. = FALSE)
  }
  
  headers <- requestHeaders(self, "ndjson")
    
  self$queue["progress"] <- lapply(self$queue["statusUrl"], function(x){
  response <- getRequest(self, x, headers)
  if(status_code(response) == 200){
    "100%"
  }
  else{
    headers(response)$`x-progress`
  }
  })
  
  self$queue
}

execDownloadBulk <- function(self, requestNumber, returnType, deleteFromQueue){
  execGetBulkStatus(self) # update progress
  
  if(is.na(self$queue[requestNumber,]$progress)){
    stop(paste("There are is no download in the queue at place", requestNumber), call. = FALSE)
  }
  else if(self$queue[requestNumber,]$progress != "100%"){
    stop(paste("Progress is not yet 100%, it is at", self$queue[requestNumber,]$progress), call. = FALSE)
  }
  else{
    bulk <- getBulk(self, self$queue[requestNumber,]$statusUrl, returnType)
    
    if(deleteFromQueue){
      self$queue <- self$queue[-requestNumber,]
    }
    
    bulk
  }
}

execRetrieveToken <- function(self, jwt, scopes, tokenURL){
  token <- postJWT(self, jwt, scopes, tokenURL)
  self$setToken(token$access_token)
  token
}

execDeleteBulkRequest <- function(self, requestNumber){
  response <- deleteRequest(self, self$queue[requestNumber,]$statusUrl)
  self$queue <- self$queue[-requestNumber,]
}

addToQueue <- function(self, url){
  headers <- requestHeaders(self, "ndjson")

  response <- getRequest(self, url, headers)
  response_headers <- headers(response)
  
  progress_response <- getRequest(self, response_headers$`content-location`, headers)
  progress_headers  <- headers(progress_response)
  
  self$queue[nrow(self$queue) + 1,] = list(url, response_headers$`content-location`, progress_headers$`x-progress`)
}
