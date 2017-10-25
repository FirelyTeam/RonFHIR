#' searchParams
#'
#' An alternative way to specify a query is by creating a searchParams object
#' and pass this to the \code{\link[=fhirClient]{fhirClient's}} searchByQuery. The searchParams class has a set of fluent calls to allow you
#' to easily construct more complex queries.
#' Based on \href{https://github.com/ewoutkramer/fhir-net-api}{the official HL7 FHIR .NET API.}
#'
#' @section Usage:
#' \preformatted{
#' query <- searchParams$new()
#'
#' query$select(elements)
#' query$where(criteria)
#' query$include(path)
#' query$orderBy(paramName, sortOrder = "asc")
#' query$limitTo(count)
#' query$countOnly()
#' query$summaryOnly()
#' query$textOnly()
#' query$dataOnly()
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{query}{A \code{searchParams} object that contains all specified search criteria.}
#'   \item{elements}{Elements defined at the root level in the Resource.}
#'   \item{criteria}{The search parameters to filter the Resources on. Each given string is a combined key/value pair (separated by '=').}
#'   \item{path}{Paths to include in the search.}
#'   \item{paramName}{Name of the parameter to order by.}
#'   \item{sortOrder}{Direction of the order. Can be asc or desc (ascending and descending).}
#'   \item{count}{The number of returned Resources per page.}
#' }
#'
#' @section Details:
#' \code{$new()} Creates a new searchParams object.
#'
#' \code{$select()} Specify the elements to be returned as part of a Resource.
#'
#' \code{$where()} Specify on which parameters to filter.
#'
#' \code{$include()} Specify the paths to include.
#'
#' \code{$orderBy()} Specify the order to return the results.
#'
#' \code{$limitTo()} Specify how many Resources should be returned in a single page of a Bundle.
#'
#' \code{$countOnly()} Specifiy to just return a count of the matching Resources, without returning the actual matches.
#'
#' \code{$summaryOnly()} Specify to return only those elements marked as "summary" in the base definition of the Resource(s).
#'
#' \code{$textOnly()} Specify to return only the "text" element, the 'id' element, the 'meta' element, and only top-level mandatory elements.
#'
#' \code{$dataOnly()} Specify to remove the text element.
#'
#' @name searchParams
#'
#' @examples
#' \dontrun{
#' # Setting up a fhirClient
#' client <- fhirClient$new("http://vonk.furore.com")
#'
#' # Creating a new searchParams object
#' query <- searchParams$new()
#' query$select(c("name", "birthDate"))$where("given:exact=Peter")$orderBy("family")
#'
#'
#' peters <- client$searchByQuery(query, "Patient")
#' # equivalent:
#' # client$search("Patient", c("_elements=name,birthDate","given:exact=Peter", "_sort=family"))
#'
#' while(!is.null(bundle)){
#'    # Do something useful
#'    peters <- client$continue(peters)
#' }
#' }
NULL


#' @export
searchParams <- R6Class("searchParams",
                        public = list(
                          select = function(elements)
                            select(self, private, elements),
                          where = function(criteria)
                            where(self, private, criteria),
                          include = function(path)
                            include(self, private, path),
                          orderBy = function(paramName, sortOrder = "asc")
                            orderBy(self, private, paramName, sortOrder),
                          limitTo = function(count)
                            limitTo(self, private, count),
                          countOnly = function()
                            countOnly(self, private),
                          summaryOnly = function()
                            summaryOnly(self, private),
                          textOnly = function()
                            textOnly(self, private),
                          dataOnly = function()
                            dataOnly(self, private),
                          toUriParamString = function()
                            toUriParamString(self,private),
                          add = function(name, value)
                              add(self, private, name, value)
                        ),
                        private = list(
                          # Private variables
                          elements = c(),
                          parameters = c(),
                          includes = c(),
                          revIncludes  = c(),
                          sort = c(),

                          # Single
                          count = NULL,
                          summaryType = NULL,
                          query = NULL,
                          text = NULL,
                          content = NULL,
                          filter = NULL,
                          contained = NULL,
                          containedType = NULL,

                          # Constants
                          #   Parameter for all resources
                          SEARCH_PARAM_TEXT = "_text",
                          SEARCH_PARAM_CONTENT = "_content",
                          SEARCH_PARAM_QUERY = "_query",

                          #   Search result parameters
                          SEARCH_PARAM_SORT = "_sort",
                          SEARCH_PARAM_COUNT = "_count",
                          SEARCH_PARAM_INCLUDE = "_include",
                          SEARCH_PARAM_REVINCLUDE = "_revinclude",
                          SEARCH_PARAM_SUMMARY = "_summary",
                          SEARCH_PARAM_ELEMENTS = "_elements",
                          SEARCH_PARAM_CONTAINED = "_contained",
                          SEARCH_PARAM_CONTAINEDTYPE = "_containedType",
                          SEARCH_PARAM_FILTER = "_filter",
                          SUMMARY_TYPES = c("true", "text", "data", "count", "false"),
                          SORT_ORDER = c("asc", "desc"),
                          SEARCH_CONTAINED = c("true", "false", "both"),
                          SEARCH_CONTAINED_TYPES = c("container", "contained"),
                          SEARCH_MODIF_DESCENDING = "-"
                        ))

select <- function(self, private, elements){
  if(missing(elements)){
    stop("elements is missing.", call. = FALSE)
  }
  private$elements <- c(private$elements, elements)
  invisible(self)
}

add <- function(self, private, name, value){
  if(name == private$SEARCH_PARAM_QUERY){
    private$query <- value
  }
  else if(name == private$SEARCH_PARAM_TEXT){
    private$text <- value
  }
  else if(name == private$SEARCH_PARAM_CONTENT){
    private$content <- value
  }
  else if(name == private$SEARCH_PARAM_COUNT){
    value <- strtoi(value)
    if(is.na(value) || value <= 0){
      stop("Value for _count must be a number greater than 0", call. = FALSE)
    }
    private$count <- value
  }
  else if(name == private$SEARCH_PARAM_INCLUDE){
    private$includes <- c(private$includes, value)
  }
  else if(name == private$SEARCH_PARAM_REVINCLUDE){
    private$revIncludes <- c(private$revIncludes, value)
  }
  else if(name == private$SEARCH_PARAM_SORT){
    private$sort <- c(private$sort, unlist(strsplit(value, ",")))
  }
  else if(name == private$SEARCH_PARAM_SUMMARY){
    if(!(value %in% private$SUMMARY_TYPES)){
      stop(paste(value, "is not a valid summaryType."))
    }
    private$summaryType <- value
  }
  else if(name == private$SEARCH_PARAM_FILTER){
    private$filter <- value
  }
  else if(name == private$SEARCH_PARAM_CONTAINED){
    if(!(value %in% private$SEARCH_CONTAINED)){
      stop(paste(value, "is not a recognized contained value"), call. = FALSE)
    }
    private$contained <- value
  }
  else if(name == private$SEARCH_PARAM_CONTAINEDTYPE){
    if(!(value %in% private$SEARCH_CONTAINED_TYPES)){
      stop(paste(value, "is not a recognized contained value"), call. = FALSE)
    }
    private$containedType <- value
  }
  else if(name== private$SEARCH_PARAM_ELEMENTS){
    private$elements <- c(private$elements, unlist(strsplit(value, ",")))
  }
  else{
    private$parameters <- c(private$parameters, paste(name, "=", value, sep = ""))
  }
  invisible(self)
}

toUriParamString <- function(self, private){
  result <- list()

  if(!is.null(private$query)){result <- c(result, paste(private$SEARCH_PARAM_QUERY, "=", private$query, sep = ""))}
  if(!is.null(private$text)){result <- c(result, paste(private$SEARCH_PARAM_TEXT, "=", private$text, sep = ""))}
  if(!is.null(private$content)){result <- c(result, paste(private$SEARCH_PARAM_CONTENT, "=", private$content, sep = ""))}
  if(!is.null(private$count)){result <- c(result, paste(private$SEARCH_PARAM_COUNT, "=", private$count, sep = ""))}
  if(!is.null(private$includes)){result <- c(result, paste(private$SEARCH_PARAM_INCLUDE, "=", private$includes, sep = ""))}
  if(!is.null(private$revIncludes)){result <- c(result, paste(private$SEARCH_PARAM_REVINCLUDE, "=", private$revIncludes, sep = ""))}
  if(length(private$sort) != 0){result <- c(result, paste(private$SEARCH_PARAM_SORT, "=", paste(private$sort, collapse = ","), sep = ""))}
  if(!is.null(private$summaryType)){result <- c(result, paste(private$SEARCH_PARAM_SUMMARY, "=", private$summaryType, sep = ""))}
  if(!is.null(private$filter)){result <- c(result, paste(private$SEARCH_PARAM_FILTER, "=", private$filter, sep = ""))}
  if(!is.null(private$contained)){result <- c(result, paste(private$SEARCH_PARAM_CONTAINED, "=", private$contained, sep = ""))}
  if(!is.null(private$containedType)){result <- c(result, paste(private$SEARCH_PARAM_CONTAINEDTYPE, "=", private$containedType, sep = ""))}
  if(length(private$elements) != 0){result <- c(result, paste(private$SEARCH_PARAM_ELEMENTS, "=", paste(private$elements, collapse = ","), sep = ""))}
  if(length(private$parameters) != 0){result <- c(result, private$parameters)}

  paste(result, collapse = "&")
}
