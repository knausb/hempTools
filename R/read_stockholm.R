
#' Read Stockholm format data
#' 
#' @param file A file in Stockholm format.
#' 
#' 
#' 
#' \url{https://en.wikipedia.org/wiki/Stockholm_format}
#' 
#' @return An invisible NULL.
#' 
#' @examples
#' # read_stockholm()
#' 
#' @export
read_stockholm <- function( file ) {
  
  my_stk <- base::scan( file,
                        what = "character",
                        sep = "\n",
                        skip = 0,
                        quiet = TRUE,
                        comment.char = "" )  
  
  my_version <- my_stk[1]
  my_stk <- my_stk[-1]
  
  my_ends <- grep("//", my_stk, fixed = TRUE)
  my_begins <- my_ends + 1
#  my_begins <- my_begins + 1 # Increment over version string.
  my_begins <- my_begins[ -length(my_begins) ]
  my_begins <- c(1, my_begins)
  
  stk_list <- split(cbind(my_begins, my_ends), f = 1:length(my_begins))
  stk_list <- lapply(stk_list, FUN = function(x){ my_stk[ x[1]:x[2] ] })
  
  # return( invisible(NULL) )
  return( stk_list )
}


# my_file <- "/media/knausb/Vining_lab/public_databases/NCBI/repeat_families/Cannbio-2-families.stk"
# my_stk <- read_stockholm( my_file )
# grep("^#=GF", my_stk[[1]], value = TRUE)
# unlist(lapply(my_stk, function(x){ grep("^#=GF TP", x, value = TRUE) }))

# my_stk[1:4]
# 
# stk_list[[2]][1:4]
# 
# grep("^#", stk_list[[1]], value = TRUE)
# 
# 
# split(mat, f = )
# 
# as.list(cbind(my_begins, my_ends))
# list(starts = my_begins[1:10], ends = my_ends[1:10])
# 
# c(my_begins[1:10], my_ends[1:10])

# 
# seq.int(my_begins, my_ends, by = 1)

