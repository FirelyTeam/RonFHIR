include <- function(self, private, path){
  if(missing(path)){
    stop("path is missing.", call. = FALSE)
  }
  private$includes <- c(private$includes, path)
  invisible(self)
}

where <- function(self, private, criteria){
  if(missing(criteria)){
    stop("criteria is missing.", call. = FALSE)
  }
  splitted <- lapply(strsplit(criteria, "="), function(x) if (length(x) == 2) {x} else {stop("Criteria must be in format parameter=value", call. = FALSE)})
  lapply(splitted, function(x) self$add(x[1], x[2]))
  invisible(self)
}

custom <- function(self, private, customQueryName){
  if(missing(customQueryName)){
    stop("customQueryName is missing.", call. = FALSE)
  }
  private$query <- customQueryName
  invisible(self)
}

orderBy <- function(self, private, paramName, sortOrder = "asc"){
  if(missing(paramName)){
    stop("paramName is missing.", call. = FALSE)
  }
  if(!(sortOrder %in% private$SORT_ORDER)){
    stop("sortOrder is invalid. sortOrder must be \"asc\" or \"desc\".", call. = FALSE)
  }
  if(sortOrder == "asc"){
    private$sort <- c(private$sort, paramName)
  }
  else{
    private$sort <- c(private$sort, paste(private$SEARCH_MODIF_DESCENDING, paramName, sep = ""))
  }
  invisible(self)
}

limitTo <- function(self, private, count){
  self$add(private$SEARCH_PARAM_COUNT, count) # this uses the 'count check', instead of private$count <- count
  invisible(self)
}

countOnly <- function(self, private){
  private$summaryType <- "count"
  invisible(self)
}

summaryOnly <- function(self, private){
  private$summaryType <- "true"
  invisible(self)
}

textOnly <-  function(self, private){
  private$summaryType <- "text"
  invisible(self)
}

dataOnly <- function(self, private){
  private$summaryType <- "data"
  invisible(self)
}
