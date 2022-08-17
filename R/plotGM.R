

#' Plot a genetic map
#' 
#' @param map An object of class "cross" from the package 'qtl' or a list of vectors.
#' 
#' 
#' 
#' 
#' @return A ggplot object.
#' 
#' @examples
#' 
#' fake_gm <- structure(
#'   list(
#'     `1` = structure(c(D1M430 = 0, D1M318 = 9.8, D1M212 = 13.11, D1M437 = 37.11,
#'                       D1M490 = 53.51, D1M15 = 79.71, D1M17 = 103.81), class = "A"),
#'     `2` = structure(c(D2M359 = 0, D2M241 = 21.8, D2M336 = 44.8, D2M398 = 50.2,
#'                       D2M51 = 72.1), class = "A"),
#'     `3` = structure(c(D3M227 = 0, D3M10 = 19.7, D3M17 = 32.8), class = "A"),
#'     `4` = structure(c(D4M286 = 0, D4M145 = 20.7, D4M12 = 36, D4M310 = 52.4), class = "A")),
#'   class = "map")
#' p <- plotGM(fake_gm)
#' p
#' p <- p + ggtitle("Best linkage map ever")
#' p
#' p <- p + annotate(geom="text", x=2.4, y=25, label="Super\nmarker",
#'                   color="red", size = 3)
#' p
#' p + annotate(geom="rect", xmin = 1.5, xmax = 2.4, ymin = 40, ymax = 60,
#'              color="red", size = 1, fill = NA)
#' 
#' 
#' 
#' 
#' 
#' 
#' @export
plotGM <- function( map ) {
  require(ggplot2)
  #max_gd <- max(unlist(lapply(map, max)))
  
  if( inherits( map, "cross" ) ){
    map <- qtl::pull.map(map)
  }
  
  marker_df <- data.frame(
    chrom = rep(names(map), times = lapply(map, length)),
    pos = unlist(map),
    marker = names(unlist(map))
  )
  marker_df$chromf <- factor( marker_df$chrom, levels = names(map) )
  marker_df$chromn <- as.numeric( marker_df$chromf )

  chr_df <- data.frame(
    start = unlist(lapply(map, min)),
    end = unlist(lapply(map, max))
  )
  chr_df$chr <- names(map)
  chr_df$chrf <- factor( chr_df$chr, levels = names(map))
  chr_df$chrn <- as.numeric( chr_df$chrf )
  
  chrom_wid <- 0.02
  p <- ggplot()
  p <- p + geom_rect( data = chr_df, 
                             aes( xmin = chrn - chrom_wid,
                                  xmax = chrn + chrom_wid,
                                  #xmin = as.numeric(as.factor(chr)) - chrom_wid,
                                  #xmax = as.numeric(as.factor(chr)) + chrom_wid,
                                  ymin = end, ymax = start), 
                      #fill = "#C0C0C0",
                      fill = "#DCDCDC",
                      #fill = "#F5F5F5",
                      color = "#000000"
                      )
  #p <- p + scale_y_reverse(limits = c(max_gd, 0))
  
  marker_wid <- 0.1
  marker_high <- 0.4
  p <- p + geom_rect( data = marker_df, 
                      aes( xmin = chromn - marker_wid,
                           xmax = chromn + marker_wid, 
                           #xmin = as.numeric(as.factor(chrom)) - marker_wid,
                           #xmax = as.numeric(as.factor(chrom)) + marker_wid,
                           ymin = pos - marker_high, ymax = pos + marker_high),
                      fill = "#228B22", color = "#228B22"
  )
  # 
  # p <- p + scale_y_reverse( breaks = seq(0, 2e3, by = 100) )
  p <- p + scale_y_reverse( minor_breaks = seq(0, 2e3, by = 20), breaks = seq(0, 2e3, by = 100) )
  #p <- p + scale_x_continuous( breaks = as.numeric(as.factor(chr_df$chr) ) )
  p <- p + scale_x_continuous( breaks = chr_df$chrn, labels = chr_df$chr )
  # p <- p + scale_y_reverse(limits = c(max_gd, 0))
  # p <- p + scale_y_reverse(limits = c(0, max_gd))
  #p <- p + theme_bw() + theme( panel.grid.minor = element_blank() )
  p <- p + theme_bw() + 
    theme( panel.grid.minor.x = element_blank(), 
           panel.grid.major.y = element_line( size = 0.4, color = "#C0C0C0", linetype = 1 ),
           panel.grid.minor.y = element_line( size = 0.4, color = "#C0C0C0", linetype = 3 )
          )
  p <- p + xlab("Chromosome")
  #p <- p + ylab("Distance (cM)")
  p <- p + ylab("Location (cM)")
  #p
  

  #p

  #return( invisible( NULL ) )
  p
}

