

#' Read BLAST custom tabular data
#' 
#' @param file A file in tabular format.
#' 
#' 
#' 
#' 
#' @return A data.frame.
#' 
#' @examples
#' # read_blast()
#' 
#' @export
read_blast <- function( file ) {
  
  colnames1 <- c("qseqid", "qlen", "sseqid", "slen", 
                 "qstart", "qend", "sstart", "send",
                 #                                    "qseq",
                 "evalue", "bitscore", "score", "length",
                 "pident", "nident", "mismatch", "positive",
                 "gapopen", "gaps", "ppos")
  
  
  #library(readr)
  require(readr)
  bout <- readr::read_csv(file = file, 
                          col_names = colnames1,
                          show_col_types = FALSE
  )
  
  return( bout )
}










