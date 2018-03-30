getResource <- function(self, url){
  json <- getJSON(self, url)
  json <- paste('{"resource": [', json, ']}', sep = "") # workaround
  fromJSON(json)$resource
}

getBundle <- function(self, url){
  json <- getJSON(self, url)
  bundle <- fromJSON(json)
  
  if(!is.null(bundle$entry$resource) && "OperationOutcome" %in% bundle$entry$resource$resourceType){
    issue <- bundle$entry$resource[which(bundle$entry$resource$resourceType == "OperationOutcome"),]$issue
    issue <- unlist(issue)
    warning(paste(names(issue), issue, sep = ": ", collapse = "\n"), call. = FALSE)
  }
  bundle
}

toGraphQLURL <- function(self, location, query){
  if(!is.null(location)){
    location <- paste(location, "/", sep = "")
  }
  paste(self$endpoint, location, "$graphql?query=", query, sep="")
}

toOperationURL <- function(self, resourceType, id, name, parameters)
{
  path <- ""
  if(!is.null(resourceType)) {path <- paste(path, resourceType, "/", sep="")}
  if(!is.null(id)) {path <- paste(path, id, "/", sep="")}
  path <- paste(path, "$", name, sep="")
  if(!is.null(parameters)) {path <- paste(path, "?", parameters, sep="")}
  paste(self$endpoint, path, sep="")
}

toSearchURL <- function(self, resourceType, criteria, includes, pageSize, summaryType, q){
  if(is.null(resourceType)){
    endpoint <- substr(self$endpoint, 1, nchar(self$endpoint) - 1) # exclude "/" at the end of the endpoint for wholesystemsearch
  }
  else{
    endpoint <- paste(self$endpoint, resourceType, sep = "")
  }
  if(is.null(q)){
    q <- searchParams$new()
    if(!is.null(criteria)){q$where(criteria)}
    if(!is.null(pageSize)){q$limitTo(pageSize)}
    if(!is.null(includes)){q$include(includes)}
    if(!is.null(summaryType)){q$where(paste("_summary=", summaryType, sep =""))}
  }
  paste(endpoint, "?", q$toUriParamString(), sep = "")
}

toReadURL <- function(self, location, summaryType){
  if(!is.null(summaryType)){summaryType <- paste("?_summary=", summaryType, sep="")}
  paste(self$endpoint, location, summaryType, sep="")
}

getJSON <- function(self, url){
  response <- tryCatch(GET(URLencode(url), 
                           accept_json(), 
                           config(token = self$token)), 
                       error = function(e){stop("Could not connect to endpoint")})
  
  content <- content(response, as = "text", encoding = "UTF-8")
  if(http_error(response)){
    stop(http_status(response)$message, call. = FALSE)
  }
  if(!validate(content)){
    stop("Server response is not in JSON format.", call. = FALSE)
  }
  content
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
