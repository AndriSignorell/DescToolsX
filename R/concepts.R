
#' Concept Utilities for Package Documentation
#'
#' Helper functions to inspect and analyse the use of 
#' `\\concept` tags within a package.
#'
#' These utilities extract concept metadata from Rd files
#' and allow structured auditing of conceptual organisation
#' inside a package.
#'
#' \strong{Functions}
#' \itemize{
#'   \item \code{getConcepts()} - Returns all unique concepts used in a package.
#'   \item \code{conceptMap()} - Returns a mapping of concepts to functions.
#'   \item \code{conceptAudit()} - Returns a summary table of concept usage.
#' }
#'
#' @param pkg character string. Name of the installed package.
#'
#' @name concepts  
#' @details
#' The functions use \code{\link[tools]{Rd_db}} to parse Rd files and
#' extract `\\concept` tags programmatically.
#'
#' These tools are intended for package development,
#' documentation consistency checks, and conceptual audits.
#'
#' @return
#' \describe{
#'   \item{\code{getConcepts}}{character vector of unique concept names}
#'   \item{\code{conceptMap}}{named list mapping concepts to functions}
#'   \item{\code{conceptAudit}}{data frame with concept frequencies}
#' }
#'
#' @examples 
#' getConcepts("DescToolsX")
#' conceptMap("DescToolsX")
#' conceptAudit("DescToolsX")
#' 
#' 



#' @rdname concepts  

#' @family utils  
#' @concept programming
#'
#'
#' @export
getConcepts <- function(pkg) {
  rd_db <- tools::Rd_db(pkg)
  
  concepts <- unlist(lapply(rd_db, function(rd) {
    concept_nodes <- Filter(function(x) attr(x, "Rd_tag") == "\\concept", rd)
    
    if (length(concept_nodes) > 0) {
      unlist(lapply(concept_nodes, as.character))
    }
  }))
  
  unique(concepts)
}




#' @rdname concepts  
#' @export
conceptMap <- function(pkg) {
  rd_db <- tools::Rd_db(pkg)
  out <- list()
  
  for (name in names(rd_db)) {
    rd <- rd_db[[name]]
    
    concept_nodes <- Filter(function(x) attr(x, "Rd_tag") == "\\concept", rd)
    
    if (length(concept_nodes) > 0) {
      concepts <- unlist(lapply(concept_nodes, as.character))
      
      for (c in concepts) {
        out[[c]] <- c(out[[c]], sub("\\.Rd$", "", name))
      }
    }
  }
  
  out
}


#' @rdname concepts  
#' @export
conceptAudit <- function(pkg) {
  
  cm <- conceptMap(pkg)
  data.frame(
    concept = names(cm),
    n_functions = lengths(cm)
  )[order(lengths(cm), decreasing = TRUE), ]
  
}

