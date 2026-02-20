
#' Parse a SAS Dataline
#' 
#' A parser for simple SAS dataline command texts. A \code{data.frame} is being
#' built with the columnnames listed in the input section. The data object will
#' be created in the given environment. 
#' 
#' The SAS function \code{DATA} is designed for quickly creating a dataset from
#' scratch. The whole step normally consists out of the \code{DATA} part
#' defining the name of the dataset, an \code{INPUT} line declaring the
#' variables and a \code{DATALINES} command followed by the values.\cr The
#' default delimiter used to separate the different variables is a space (thus
#' each variable should be one word). The $ after the variable name indicates
#' that the variable preceding contain character values and not numeric values.
#' Without specific instructions, SAS assumes that variables are numeric. The
#' function will fail, if it encounters a character in the place of an expected
#' numeric value.\cr Each new row in datalines will create a corresponding
#' unique row in the dataset. Notice that a ; is not needed after every row,
#' rather it is included at the end of the entire data step.
#' 
#' More complex command structures, i.e. other delimiters (dlm), in the
#' \code{INPUT}-section are not (yet) supported. 
#' 
#' @param x the SAS text
#' @param env environment in which the dataset should be created.
#' @param overwrite logical. If set to TRUE, the function will silently
#' overwrite a potentially existing object in \code{env} with the same name as
#' declared in the SAS \code{DATA} section. If set to \code{FALSE} (default) an
#' error will be raised if there already exists an object with the same name.
#' 
#' @return a data.frame
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso \code{\link{scan}}
#' @keywords IO
#' @examples
#' 
#' txt <- "
#' DATA asurvey;
#' INPUT id sex $ age inc r1 r2 r3 ;
#' DATALINES;
#' 1   F  35 17  7 2 2
#' 17  M  50 14  5 5 3
#' 33  F  45  6  7 2 7
#' 49  M  24 14  7 5 7
#' 65  F  52  9  4 7 7
#' 81  M  44 11  7 7 7
#' 2   F  34 17  6 5 3
#' 18  M  40 14  7 5 2
#' 34  F  47  6  6 5 6
#' 50  M  35 17  5 7 5
#' ;
#' "
#' 
#' (d.frm <- parseSASDatalines(txt))
#' 


#' @export
parseSASDatalines <- function(x, env = .GlobalEnv, overwrite = FALSE) {
  
  # see: http://www.psychstatistics.com/2012/12/07/using-datalines-in-sas/
  # or:  http://www.ats.ucla.edu/stat/sas/library/SASRead_os.htm
  
  # split command to list by means of ;
  lst <- strTrim(strsplit(x, ";")[[1]])
  dsname <- lst[grep(pattern = "^[Dd][Aa][Tt][Aa] ", strTrim(lst))]   # this would be the dataname
  dsname <- gsub(pattern = "^[Dd][Aa][Tt][Aa] +", "", dsname)
  
  # get the columnnames from the input line
  input <- lst[grep(pattern = "^[Ii][Nn][Pp][Uu][Tt]", strTrim(lst))]
  # get rid of potential single @
  input <- gsub("[ \n\t]@+[ \n\t]*", "", input)
  input <- gsub(pattern=" +\\$", "$", input)
  input <- gsub(" +", " ", input)
  cnames <- strsplit(input, " ")[[1]][-1]
  
  # the default values for the variables
  def <- rep(0, length(cnames))
  def[grep("\\$$", cnames)] <- "''"
  vars <- paste(gsub("\\$$","",cnames), def, sep="=", collapse=",")
  
  datalines <- lst[grep("datalines|cards|cards4", tolower(lst))+1]
  
  fn <- textConnection(datalines)
  res <- eval(parse(text=gettextf(
    "data.frame(scan(file=(fn),
    what=list(%s), quiet=TRUE))", vars)))
  
  close(fn)
  
  if(length(dsname) > 0){ # check if a dataname could be found
    if( overwrite | ! exists(dsname, envir=env) ) {
      assign(dsname, res, envir=env)
      
      note <- cli::col_cyan(gettextf("\nThe object %s has been added to %s.\n" 
                                , dsname, deparse(substitute(env)))) 
      cat(note)
      
    } else {
      cat(gettextf("The object %s already exists in %s. Should it be overwritten? (y/n)\n"
                   , dsname, deparse(substitute(env))))
      ans <- readline()
      if(ans == "y"){
        assign(dsname, res, envir = env)
        
        note <- cli::col_cyan(gettextf("\nThe object %s has been overwritten in %s.\n" 
                                  , dsname, deparse(substitute(env)))) 
        cat(note)
      }
      
      # stop(gettextf("%s already exists in %s. Use overwrite = TRUE to overwrite it.", dsname, deparse(substitute(env))))
    }
  }
  
  return(res)
  
}


