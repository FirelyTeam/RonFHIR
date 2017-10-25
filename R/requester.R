getResource <- function(private, location, summaryType){
  if(!is.null(summaryType)){summaryType <- paste("?_summary=", summaryType, sep="")}
  url <- paste(private$endpoint, location, summaryType, sep="")
  json <- getJSON(url)
  json <- paste('{"resource": [', json, ']}', sep = "") # workaround
  fromJSON(json)$resource
}

getBundle <- function(private, resourceType, criteria, includes, pageSize, summaryType, q){
  if(is.null(resourceType)){
    endpoint <- substr(private$endpoint, 1, nchar(private$endpoint) - 1) # exclude "/" at the end of the endpoint for wholesystemsearch
  }
  else{
    endpoint <- paste(private$endpoint, resourceType, sep = "")
  }
  if(is.null(q)){
    q <- searchParams$new()
    if(!is.null(criteria)){q$where(criteria)}
    if(!is.null(pageSize)){q$limitTo(pageSize)}
    if(!is.null(includes)){q$include(includes)}
    if(!is.null(summaryType)){q$where(paste("_summary=", summaryType, sep =""))}
  }

  url <- paste(endpoint, "?", q$toUriParamString(), sep = "")
  json <- getJSON(url)
  fromJSON(json)
  bundle <- fromJSON(json)

  if(!is.null(bundle$entry$resource) && "OperationOutcome" %in% bundle$entry$resource$resourceType){
      issue <- bundle$entry$resource[which(bundle$entry$resource$resourceType == "OperationOutcome"),]$issue
      issue <- unlist(issue)
      warning(paste(names(issue), issue, sep = ": ", collapse = "\n"), call. = FALSE)
  }

  bundle
}

getJSON <- function(url){
  response <- tryCatch(GET(URLencode(url), accept_json()), error = function(e){stop("Could not connect to endpoint")})
  content <- content(response, as = "text", encoding = "UTF-8")
  if(http_error(response)){
    stop(http_status(response)$message, call. = FALSE)
  }
  if(!validate(content)){
    stop("Server response is not in JSON format.", call. = FALSE)
  }
  content
}
