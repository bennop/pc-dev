require(jpeg)
require(rasterImage)
require(lubridate)

rect.compare <- function(img = '/Users/puetz/Pictures/2019/08/2019_08_26\ UWW3/UWW3/Output/compare/2019_08_26-16_29_25 EOS 5D Mark IV-023126_bp_full.jpg',
                         base.f = 11,

                         fs = c(11, 15, 24, 35, 50, 70, 100, 135,200,400, 800),
                         rcol = 'white',
                         acol = 'lightgrey',
                         file = 'rectcomp.jpg',
                         ...){
    base.img <- readJPEG(img)
    dims <- dim(base.img)
    text.scale <- 6720/dims[2]

    #print(dims)

    fs <- fs[fs >= base.f] # only those make sense
    angles <-  round(sapply(c(24,36, sqrt(24^2+36^2)),
                            function(d)((pi - 2*atan2(2*fs, d)) * 180/pi)),
                     2)
    colnames(angles) <- c('s', 'l', 'd')
    rownames(angles) <- fs
    f.ratios <- base.f/fs
    ctr <- dims[1:2]/2
    dir <- dirname(img)
    if (!(is.null(file) | file == '')){
        outfile <- file.path(dir, file)
        message('writing to ', outfile)
        jpeg(outfile,
             width = dims[2],
             height = dims[1],
             ...)
        need.close <- TRUE
    } else {
        need.close <- FALSE
    }
    plot(c(0, (dims[2]-1)),
         c(0, (dims[1]-1)),
         type = 'n',
         xaxs = 'i', yaxs = 'i',
         axes = FALSE,
         xlab = '', ylab = '',
         mar = rep(0,4))
    rasterImage(base.img,
                0,
                0,
                dims[2]-1,
                dims[1]-1)
    rect(ctr[2]*(1 - f.ratios),
         ctr[1]*(1 - f.ratios),
         ctr[2]*(1 + f.ratios) - 1,
         ctr[1]*(1 + f.ratios) - 1,
         border = rcol,
         lwd = c(1,1,2,1,1,2,1,1,2,1,1))
    text(ctr[2]*(1 - f.ratios),
         ctr[1]*(1 + f.ratios) - 1,
         paste(fs, c(rep('mm', length(fs)-1),'')),
         adj = 0:1,
         col = rcol,
         cex = 2.5 * text.scale,
         ...)
    # horizontal angles
    text(ctr[2],
         ctr[1]*(1 - f.ratios) + 1,
         paste0(angles[, ifelse(dims[1]>dims[2],
                                's',
                                'l')],
                "°"),
         adj = c(0.5,0),
         col = acol,
         cex = 1.6 * text.scale,
         ...)
    # vertical angles
    text(ctr[2]*(1 + f.ratios) - 2,
         ctr[1],
         paste0(angles[, ifelse(dims[1]<dims[2],
                                's',
                                'l')],
                "°"),
         adj = c(0.5, 0),
         col = acol,
         cex = 1.6 * text.scale,
         srt = 90,
         ...)
    for(i in 1:0){
        offset <- 1
        text(ctr[2]+i*offset, 100-i*offset,
             paste0("©", year(today()), " Benno Pütz - All rights reserved      bpfoto@online.de"),
             cex = 2 * text.scale,
             col = c('lightgrey', 'black')[i+1],
             adj = c(0.5, 0))
    }
    if (need.close){
        message('closing')
        dev.off()
    }
}
