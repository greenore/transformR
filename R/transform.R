# Transformation Functions
#-------------------------

#' @title Transformation of German umlaute
#' @export
#' 
#' @description \code{changeLevels} Transform all the "ae's", "oe's" and "ue's".
#'  
#' @param charVector A character vector of unspecified length
#' 
#' @examples
#' changeUmlaute(names(df))
#'  

changeUmlaute <- function(charVector){
  
  # Ae's
  charVector <- gsub('\u00C4', 'Ae', charVector)
  charVector <- gsub('\u00E4', 'ae', charVector)
  
  # Oe's
  charVector <- gsub('\u00D6', 'Oe', charVector)
  charVector <- gsub('\u00F6', 'oe', charVector)
  
  # Ue's
  charVector <- gsub('\u00DC', 'Ue', charVector)
  charVector <- gsub('\u00FC', 'ue', charVector)
  
  # E-acute
  charVector <- gsub('\u00C9', 'E', charVector)
  charVector <- gsub('\u00E9', 'e', charVector)
  
  # E-grave
  charVector <- gsub('\u00c8', 'E', charVector)
  charVector <- gsub('\u00e8', 'e', charVector)
  
  # E-circumflex
  charVector <- gsub('\u00ca', 'E', charVector)
  charVector <- gsub('\u00ea', 'e', charVector)
  
  # A-circumflex
  charVector <- gsub('\u00c2', 'A', charVector)
  charVector <- gsub('\u00e2', 'a', charVector)
  
  # I-circumflex
  charVector <- gsub('\u00ce', 'I', charVector)
  charVector <- gsub('\u00ee', 'i', charVector)
  
  # C-cedilla
  charVector <- gsub('\u00c7', 'C', charVector)
  charVector <- gsub('\u00e7', 'c', charVector)
  
  charVector
}

#' @title Transformation of factor levels
#' @export
#' 
#' @description \code{changeLevels} 
#'  
#' @param charVector
#' 

changeLevels <- function(data, var_name, old_level, new_level){
  
  if(is.factor(data[, var_name])){
    levels(data[, var_name])[levels(data[, var_name]) == old_level] <- new_level
  }
  
if(is.character(data[, var_name])){
    data[, var_name][data[, var_name] %in% c(old_level)] <- new_level
  }

if(is.numeric(data[, var_name])){
    warning("Variable is numeric... \nNo transformation applied")
  }

  data[, var_name]
}

#' @title Transformation from factors to numeric variables
#' @export
#' 
#' @description \code{fact2Num} 
#'  
#' @param data
#' @param var
#' 

fact2Num <- function(data, var){
  as.numeric(as.character(data[, var]))
}

#' @title Transformation from factors to date variables
#' @export
#' 
#' @description \code{fact2Date} 
#'  
#' @param data
#' @param var
#' 

fact2Date <- function(data, var){
  as.Date(levels(data[, var])[data[, var]])
}

#' @title Assign NA for blank values ("") for factor variables
#' @export
#' 
#' @description \code{blank2Missing} 
#'  
#' @param data
#' 

blank2Missing <- function(data){
  
  for (i in names(data)){
    if (class(data[, i]) == 'factor'){
      data[, i][data[, i] == ""] <- NA
      levels(data[, i])[levels(data[, i]) == ""] <- NA
    }
  }
  
  return(data)
}

#' @title Transform a long function text into a formula
#' @export
#' 
#' @description \code{textFun} 
#'  
#' @param yvar
#' @param xvar
#'

textFun <- function(yvar, xvar){
  yvar <- paste(yvar, ' ~ ', sep = '')
  xvar <- paste(xvar, sep = '', collapse = ' + ')
  fun <- paste(yvar, xvar, sep = '')
  paste(fun)
}

#' @title Cuting folder files
#' @export
#' 
#' @description \code{cutFiles} Import a file list from a folder and cut it into
#' pieces
#'  
#' @param path
#' @param cut_left
#' @param cut_right
#'

cutFiles <- function(path, cut_left, cut_right){
  files <- list.files(path, full.names = F)
  
  files <- substr(x = files,
                  start = nchar(cut_left) + 1,
                  stop = nchar(files) - nchar(cut_right))
  
  files <- as.Date(files, "%Y-%m-%d")
  
  return(files)
}

#' @title Return the newest file from a folder w the cutFiles function
#' @export
#' 
#' @description \code{newestFile}
#'  
#' @param path
#' @param cut_left
#' @param cut_right
#'

newestFile <- function(path, cut_left, cut_right){
  files <- cutFiles(path, cut_left, cut_right)
  
  x <- paste(cut_left, files[files %in% max(files, na.rm = T)], sep = '')
  x <- paste(x, cut_right, sep = '')
  
  return(x)
}
