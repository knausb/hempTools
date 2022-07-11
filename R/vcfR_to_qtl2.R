

#' Convert vcfR to qtl2 format
#' 
#' @param vcf A vcfR object
#' @param prefix A prefix for file names
#' @param geno_file Create a geno_file for qtl2
#' @param pmap Create a pmap file (physical map)
#' @param transpose transpose the VCF data
#' @param verbose report verbose messages
#' 
#' 
#' @return An invisible NULL.
#' 
#' @examples
#' # vcfR_to_qtl2()
#' 
#' @export
vcfR_to_qtl2 <- function( vcf, 
                          prefix = "MYDATA",
                          geno_file = TRUE, 
                          pmap = TRUE,
                          transpose = FALSE,
                          verbose = TRUE ) {
  
  my_bialleles <- vcfR::is.biallelic(vcf)
  vcf <- vcf[my_bialleles, ]
  
  gt <- extract.gt(vcf)
  
  if( geno_file == TRUE){
    gt <- cbind(rownames(gt), gt)
    gt <- rbind(colnames(gt), gt)
    gt[1, 1] <- "SampleName"
    
    if( transpose == TRUE ){
      gt <- base::t(gt)
    }
    
    write.table(gt,
                paste(prefix, "_geno_file.csv", sep = ""),
                sep = ",",
                row.names = FALSE, col.names = FALSE,
                quote = FALSE)
  }

  if( pmap == TRUE ){
    pmap <- cbind(colnames(gt)[-1], vcf@fix[ , 1:2])
    colnames(pmap) <- c("marker", "chr", "pos")
    pmap[, "pos"] <- as.numeric(pmap[, "pos"])/1e6
    
    write.table(pmap,
                paste(prefix, "_pmap_file.csv", sep = ""),
                sep = ",",
                row.names = FALSE,
                col.names = TRUE,
                quote = FALSE)
  }  
  
  
  if( verbose == TRUE ){  
    control_msg <- 'Include the following in the control file:'
    control_msg <- c(control_msg, '\ngeno_codes = structure(1:3, names = c("0/0", "0/1", "1/1")),')
    if( transpose == TRUE ){
      control_msg <- c(control_msg, '\ngeno_transposed = FALSE,')
    }
    if( transpose == FALSE ){
      control_msg <- c(control_msg, '\ngeno_transposed = TRUE,')
    }
    message( control_msg )
  }
  
  return( invisible( NULL ) )
}


