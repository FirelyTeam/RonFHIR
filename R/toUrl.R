toExportUrl <- function(self, resourceType, groupId, criteria){
  path <- ""
  if(!is.null(resourceType)){
    path <- paste(resourceType, "/$export", sep = "")
  }
  else if(!is.null(groupId)){
    path <- paste("Group/", groupId, "/$export", sep = "")
  }
  else{
    path <- "$export"
  }
  
  q <- searchParams$new()
  
  if(!is.null(criteria)){
    q$where(criteria)
  }
  
  paste(self$endpoint, path, "?", q$toUriParamString(), sep = "")
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
  if(!is.null(summaryType)){
    summaryType <- paste("?_summary=", summaryType, sep="")
  }
  paste(self$endpoint, location, summaryType, sep="")
}