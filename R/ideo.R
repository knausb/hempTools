
#' Create an ideogram
#' 
#' @param chroms A data.fram describing the chromosomes.
#' 
#' @return An invisible NULL.
#' 
#' @examples
#' ideo()
#' 
#' @export
ideo <- function( chroms = NULL ) {

  mycols <- c("#0D0887FF", "#FCA536FF", "#BD3786FF", "#3E049CFF", "#E26560FF", 
              "#DE6065FF", "#F0F921FF", "#C03A83FF", "#8E0CA4FF", "#F79044FF")
  
  nuc <- structure(
    list(
      Id = c("NC_044371.1", "NC_044375.1", "NC_044372.1", "NC_044373.1",
             "NC_044374.1", "NC_044377.1", "NC_044378.1", "NC_044379.1", 
             "NC_044376.1", "NC_044370.1"),
      Length = c(101209240L, 96346938L, 94670641L, 91913879L, 88181582L,
                 79335105L, 71238074L, 64622176L, 61561104L, 104987320L),
      gc = c(0.33021597973084, 0.336686836019109, 0.333845160115941,
             0.331067297493258, 0.335114019198317, 0.335004814109969, 
             0.338228180243084, 0.333975826130554, 0.332627747302403,
             0.336158176700696),
      chrom = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      ),
    row.names = c(NA, 10L),
    class = "data.frame")
  
  plot(1:nrow(nuc), nuc$Length, type = "n",
       xlab = "", ylab = "",
       ylim = c(0, max(nuc$Length)),
       xaxt = "n", yaxt = "n",
       frame.plot = FALSE)
  
  axis(side = 1, at = 1:nrow(nuc), labels = c(1:9, "X"), 
       mgp = c(3, 0.4, 0), tcl = -0.2)
  title(xlab = "Chromosome", line = 1.4)
  axis(side = 2, at = seq(0, 100e6, by = 10e6), labels = (0:10)*10, las = 2)
  title(ylab = "Position (Mbp)")
  abline(h = seq(0, 100e6, by = 10e6), col = "#C0C0C088")

  chrmw <- 0.05
  
  rect(xleft  = 1:nrow(nuc) - chrmw,
       ybottom = rep(1, times = nrow(nuc)), 
       xright = 1:nrow(nuc) + chrmw,
       ytop   =  nuc$Length[1:nrow(nuc)],
       col = mycols,
       border = "#808080")
  
  return( invisible(NULL) )    
}



