getRequest <- function(self, url, headers){
  response <- tryCatch(GET(URLencode(url), 
                           add_headers(headers)),
                       error = function(e){stop("Could not connect to endpoint")})
  
  if(http_error(response)){
    stop(paste(http_status(response)$message, content(response, as = "text", encoding = "UTF-8"), sep = "\n"), call. = FALSE)
  }
  
  response
}

requestHeaders <- function(self, returnType){
  headers <- c(Authorization = self$token)
  
  if(returnType == "ndjson"){
    headers <- c(headers, Accept = "application/fhir+json", Prefer = "respond-async") # application/fhir+ndjson not supported yet
  }
  else if(returnType == "xml"){
    headers <- c(headers, Accept = "application/fhir+xml")
  }
  else{
    headers <- c(headers, Accept = "application/fhir+json")
  }
  
  headers
}

getResource <- function(self, url, returnType){
  headers <- requestHeaders(self, returnType)
  response <- getRequest(self, url, headers)
  payload <- content(response, as = "text", encoding = "UTF-8")
  
  if(returnType %in% c("json", "xml")){
    payload
  }
  else{
    fromJSON(payload)
  }
}

getBulk <- function(self, url, returnType){
  # get download links
  downloadOverview <- getResource(self, url, "parsed")
  
  headers <- requestHeaders(self, returnType)
  
  result <- lapply(downloadOverview$output$url, function(x){
    response <- getRequest(self, x, headers)
    content <- content(response, as = "text", encoding = "UTF-8")
    if(returnType == "ndjson"){
      content
    }
    else{
      fromNDJSON(content)
    }
  })
  names(result) <- downloadOverview$output$type
  c(downloadOverview, result)
}

fromNDJSON <- function(ndjson){
  splitted <- strsplit(ndjson, "\n")[[1]]
  result <- list()
  for(i in 1:length(splitted)){
    x <- fromJSON(splitted[i])
    id <- x$id
    if(!is.null(id)){
      result[[x$id]] <- x
    }
    else{
      result[[i]] <- x
    }
  }
  result
}

# JSON only for now
putResource <- function(self, resource){
  if(!validate(resource)){
    stop("Resource is not in JSON format.", call. = FALSE)
  }
  parsed <- fromJSON(resource)
  resourceType <- try(parsed$resourceType)
  id <- try(parsed$id)
  if(is.null(resourceType) || is.null(id)){
    stop("The resourceType and/or id is not set properly.", call. = FALSE)
  }
  
  path <- paste(self$endpoint, resourceType, '/', id, sep = "")
  response <- PUT(path, body = resource, content_type_json())
  
  if(http_error(response)){
    stop(http_status(response)$message, call. = FALSE)
  }
}

postJWT <- function(self, jwt, scopes, tokenURL){
  url <- ""
  if(!is.null(self$tokenURL)){
    url <- self$tokenURL
  }
  else if(!is.null(tokenURL)){
    url <- tokenURL
  }
  else{
    stop("No tokenURL is set", call. = FALSE)
  }
  
  response <- tryCatch(POST(URLencode(url), 
                            body = list(grant_type = "client_credentials",
                                        client_assertion_type = "urn:ietf:params:oauth:client-assertion-type:jwt-bearer",
                                        client_assertion = jwt,
                                        scope = paste(scopes, collapse = " ")),
                            encode = "form"),
                       error = function(e){stop("Could not connect to endpoint")})
  
  if(http_error(response)){
    stop(paste(http_status(response)$message, content(response, as = "text", encoding = "UTF-8"), sep = "\n"), call. = FALSE)
  }
  
  fromJSON(content(response, as = "text", encoding = "UTF-8"))
}

deleteRequest <- function(self, url){
  response <- tryCatch(DELETE(URLencode(url)),
                       error = function(e){stop("Could not connect to endpoint")})
  if(http_error(response)){
    stop(http_status(response)$message, call. = FALSE)
  }
  
  response
}