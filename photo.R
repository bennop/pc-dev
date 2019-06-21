## f: focal length of lens        [mm]
## F: F-stop
## d: distance (set on lens)      [m]
## COC: circle of confusion       [mm]


##' Simple knit and typeset
##'
##' simplify typesetting process outside of RStudiox
##' @title go
##' @return none
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
##' @param n number of LaTeX runs
##' @param bib run BibTeX? (not yet implemented)
##' @param index create index? (not yet implemented)
go <- function(n=1,
               bib = FALSE,
               index = FALSE
               target = 'photo'){
    require (knitr)
    od <- setwd('~/Work/git/Photo')
    on.exit(setwd(od))
    knit(paste0(target,'.Rnw'))
    for (i in 1:n){
        system(paste('pdflatex', target))
    }
}

#
#
##' Standard F stops
##'
##' F-stops in steps of 1/3, setting \code{by} to 3 or 6 gives steps of 1 (i.e., half the light) or 2 (one quarter per step).
##'
##' Starting and final aperture are inserted if they are not part of the requested series.
##' ##' @title f.values
##' @param from maximum (starting) aperture
##' @param to minimum aperture
##' @param by stride  in sequence to return, see details
##' @return vector of F-stops
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
f.values <- function(from = 1.0,
                     to   = 22,
                     by   =  1){
    Fseq <- c( 1.0,  1.1,  1.2,  1.4,  1.6,  1.8,
               2.0,  2.2,  2.5,  2.8,  3.2,  3.5,
               4.0,  4.5,  5.0,  5.6,  6.3,  7.1,
               8.0,  9.0, 10.0, 11.0, 13.0, 14.0,
              16.0, 18.0, 20.0, 22.0, 25.0, 28.0,
              32.0, 36.0, 40.0, 44.0, 50.0, 56.0,
              64.0)
    fs.by <- Fseq[seq(1, length(Fseq), by=by)]

    fss <- unique(sort(c(from, to, fs.by)))

    return(fss[fss >= from & fss <= to])
}


##' Depth of Field (DOF)
##'
##' .. content for \details{} ..
##' @title dof
##' @param d focussing distance    x[m]
##' @param f focal length of  lens [mm]
##' @param F aperture
##' @param COC circle of confusion [mm]
##' @param verbose
##' @return two-element vector with near and far focussing limits [m]
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
dof <- function(d,
                f   = 85,
                F   = 1.2,
                COC = 0.03,
                verbose = getOption('verbose')){
    H <- HFD(f, F, COC) * 1000       # convert to [mm]
    dmm <- d * 1000                  # convert to [mm]
    n <- H * d                       #  [m * mm] here
    dd <- dmm - f                    #  [mm]
    near <- n/(H+dd)                 # back to [m]
    far  <- n/(H-dd)
    if(far<0) far <- Inf             # beyond hyperfocal distance, -> Inf
    if(verbose){
        cat(sprintf("%f - %f - %f\n",
                    round(near,4),
                    round((far-near),4),
                    round(far,4)))
    }
    return(c(near,far))
}

# near/far focus
#
##' Get near and far limits for a single aperture (given focal length and CoC)
##'
##' .. content for \details{} ..
##' @title get near and far limits
##' @param f focal length of  lens [mm]
##' @param F aperture
##' @param min minimum distance
##' @param max maximum distance
##' @param COC circle of confusion
##' @param ... not used
##' @return dataframe with columns distance, near, far, range
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
get.nf <- function(f,
                   F,
                   min = 1,
                   max = 100,
                   COC = 0.03,
                   ...){
    H <- HFD(f, F, COC)
    x <- min:max
    nf <- data.frame(x,
                     t(sapply(x,
                              FUN = dof,
                              f   = f,
                              F   = F,
                              COC = COC)))
    if(max>H){
        cat(sprintf("HFD(%dmm,%2g): %.2fm\n",f, F, H))
    }
    dimnames(nf) <- list(x,
                         c('dist', 'near', 'far'))
    nf <- within(nf,
                 range <- far-near)
    return(nf)
}

##' Get near and far limits for a set of apertures (given focal length and CoC)
##'
##' .. content for \details{} ..
##' @title  Get near and far limits
##' @param f focal length of  lens [mm]
##' @param F0 starting (maximum) aperture (smallest F number)
##' @param Fn final (minimum) aperture (largest F number)
##' @param ... pass to \code{\link{get.all.nf}}x
##' @return array with slices for aperture and columns distance, near, far, range for each
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
get.all.nf <- function(f, F0, Fn=22, ...){
    Fs <- f.values(F0, Fn, by=3)
    sq <- seq_along(Fs)
    cols <- rainbow(length(sq), end = 0.6)
    all.nf <- sapply(Fs,
                     function(F) as.matrix(get.nf(f, F, ...)),
                     simplify = 'array')
    dimnames(all.nf) <- list(distance = all.nf[,1,1],
                             focus    = c('dist', 'near', 'far','range'),
                             aperture = Fs)
    return(all.nf)
}

##' Plot near and far limits vs. focussing distance for a single aperture
##'
##' .. content for \details{} ..
##' @title Near/far plot
##' @param nf
##' @param f focal length of  lens [mm]
##' @param add
##' @param ...
##' @return
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
plot.nf <- function(nf, f=NULL, add = FALSE, ...){
    if (!add){
        plot(c(min(nf[,1]), max(nf[,1])),
             c(min(nf[,3]), min(max(nf[,3]), 4*max(nf[,1]) )),
             type = 'n',
             xlab = 'Focus Distance',
             ylab = 'Focus Range',
             main = sprintf('Near and far focusing limits [f=%dmm]', f),
             las  = 1,
             ...)
        lines(nf[,1], nf[,1], lty = 2, col  = 'lightgray')
    }
    lines(nf[,1], nf[,2], ...)
    lines(nf[,1], nf[,3], ...)
}

##' Plot near and far limits vs. focussing distance for a set of apertures
##'
##' .. content for \details{} ..
##' @title Full near/far plot
##' @param f   focal length of  lens [mm]
##' @param F0  starting (maximum) aperture (smallest F number)
##' @param Fn  final (minimum) aperture (largest F number)
##' @param ...
##' @param maxfr not used
##' @return
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
full.nf.plot <- function(f, F0, Fn=22, ..., maxfr=NULL){
    Fs <- f.values(F0, Fn, by=3)
    sq <- seq_along(Fs)
    cols <- rainbow(length(sq),
                    end = 0.6)
    all.nf <- get.all.nf(f, F0, Fn=22, ...)

    new <- TRUE
    for (i in c(max(sq),sq)){
        plot.nf(all.nf[,,i],
                f   = f,
                add = !new,
                ...,
                col = cols[i])
        if(new) {
            grid()
            pu4 <- par('usr')[4]
            new <- FALSE
        }
        H <- HFD(f, Fs[i], ...)
        #if(max(all.nf[,4,i])>ifelse(par('ylog'),10^pu4, pu4)){
        rug(H,
            ticksize = -0.02,
            side     = 3,
            col      = cols[i],
            lwd      = 1)
        #}
    }
    legend('topleft',
           legend = dimnames(all.nf)[[3]],
           title  = "F",
           lwd    = 1,
           col    = cols,
           bg     = "#FFFFFFa0")
    return(invisible(all.nf))
}

## DOF plots
##
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Single
##' @param nf
##' @param f focal length of  lens [mm]
##' @param add add to  existing plot? Otherwise  set  up plot
##' @param y.ext
##' @param ...
##' @return
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
dof.plot <- function(nf, f, add = FALSE, y.ext = 1, ...){
    if (!add){
        plot(c(min(nf[,1]), max(nf[,1])),
             c(min(nf[,4]), min(max(nf[,4])* y.ext, 4* max(nf[,1]))),
             type = 'n',
             xlab = 'Focus Distance [m]',
             ylab = 'Depth [m]',
             main = sprintf('Depth of Field [f=%dmm]', f),
             las  = 1,
             ...)

    }
    lines(nf[,1], nf[,4], ...)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title  Full DOF plot
##' @param f focal length of  lens [mm]
##' @param F0 starting (maximum) aperture (smallest F number)
##' @param Fn final (minimum) aperture (largest F number)
##' @param maxfr
##' @param y.ext
##' @param ...  pass to \code{\link{all.nf}}, \code{\link{HFD}}, \code{\link{dof.plot}}
##' @return invisibly return \code{nf} array (see \code{\link{get.all.nf}})
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
full.dof.plot <- function(f,
                          F0,
                          Fn=22,
                          maxfr=NULL,
                          y.ext = 1,
                          ...){
    Fs <- f.values(F0, Fn, by = 3)      # in full stops
    sq <- seq_along(Fs)
    cols <- rainbow(length(sq), end = 0.6)
    all.nf <- get.all.nf(f, F0, Fn, ...)

    new <- TRUE
    for (i in sq){
        dof.plot(all.nf[,,i],
                 f     = f,
                 add   = !new,
                 y.ext = y.ext,
                 ...,
                 col   = cols[i])
        if(new) {
            grid()
            pu4 <- par('usr')[4]
            new <- FALSE
        }

        ## if(max(all.nf[,4,i])>ifelse(par('ylog'),10^pu4, pu4)){

        ## add mark on upper edge to indicate HFD
        H <- HFD(f,Fs[i], ...)
        rug(H,
            ticksize = -0.02,
            side     = 3,
            col      = cols[i],
            lwd      = 1)
        #}
    }
    legend('topleft',
           legend = dimnames(all.nf)[[3]],
           title  = "F",
           lwd    = 1,
           col    = cols,
           bg     = "#FFFFFFa0")        # semitransparent white
    return(invisible(all.nf))
}


## Hyperfocal distance
##
##' Calculate the hyperfocal distance
##'
##' For a given focal length  and aperture the hyperfocal distance is the distance where the far focussing limit reaches \eqn{\infty}{infinity}. In other words, it is the shortest focussing distance where infinitely far objects appear focussed. The corresponding near focussing limit is HFD/2.
##' @title Hyperfocal distance
##' @param f focal length of  lens [mm]
##' @param F aperture
##' @param COC
##' @param ...
##' @return HFD in [m]
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
HFD <- function(f,
                F,
                COC = 0.03,
                ...){
    return((f^2/(F*COC) + f)/1000)  # convert to [m] !
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param f focal length of  lens [mm]
##' @param ... pass to \code{\link{f.values}}, \code{\link{points}}, \code{\link{plot}}
##' @return none
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
HFD.plot <- function(f,           # [mm]
                     F0  = 1,
                     COC = 0.03,  # [mm]
                     add = FALSE,
                     ...){
    Fseq <- f.values(1, ...)            # start at 1 to have the marks consistent
    pc <- rep(c('+','.','.'),
              length = length(Fseq))
    use <- Fseq>=F0                     # subset here
    useF <- Fseq[use]
    H <- HFD(f, useF, COC)

    if(add){
        points(useF, H,
               pch = pc[use],
               cex = 2,
               ...)
    } else {
        max.H.m <- max(H)
        plot(useF, H,
             log  ='xy',
             pch  = pc[use],
             cex  = 2,
             xlim = range(Fseq),
             ylim = c(0.5, max.H.m),
             main = 'Hyperfocal Distance',
             xlab = 'Aperture',
             ylab = 'HD [m]',
             las  = 1,
             axes = FALSE,
             ...)
        pow.2 <- 2^(0:7)                # powers of 2
        # main ticks
        axis(1,
             at     = pow.2,
             labels = pow.2)
        # minor ticks (adjust position of text?)
        axis(1,
             at       = pow.2*1.4,
             labels   = sprintf("%.2g", pow.2*1.4),
             lwd      = 0.5,
             cex.axis = 0.6)

        abline(v   = pow.2,
               col = "lightgray",
               lty = "dotted",
               lwd = par("lwd"))
        #
        d.lab <- as.vector(outer(c(1,2,5),
                                 10^seq(0, ceiling(log10(max.H.m)))))
        axis(2,
             at     = d.lab,
             labels = d.lab,
             las    = 1)
        abline(h   = d.lab,
               col = "lightgray",
               lty = "dotted",
               lwd = par("lwd"))
    }
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Hyperfocal plot
##' @param ... passed to \code{\link{HFD.plot}}
##' @return none
##' @author Benno Pütz \email{puetz@@psych/mpg.de}
Hyperfocal.plot <- function(...){
    ## list of focal lengths (f) and corresponding maximum apertures (F)
    ## for some interesting lenses (zoom lenses are treated as two
    ## lenses with shortest and longest focal length)
    ##
    ## this list could be factored out (similar to f.values) to be
    ## more flexible
    lenses <- cbind(f = c(400, 200, 100,  85,  70,  50,  24, 17, 8),
                    F = c(5.6, 2.8,   4, 1.2, 2.8, 1.4, 2.8,  4, 4))

    n <- nrow(lenses)                   # number of lenses
    f.lengths <- rev(lenses[,1])        # focal lengths
    nfl <- nchar(f.lengths)             #
    max.nfl <- max(nfl)                 # max. number of chars in f.len strings
    fills <- sapply(nfl,
                    function(i)substr('        ',
                                      1,
                                      2*(max.nfl - (i-1))))
    fl.str <- paste0(fills, f.lengths)  # format so that numbers are right
                                        # aligned in legend
    cols <- rainbow(n,                  # rainbow colors for different lenses
                    end = 0.6)
    for(i in 1:n){
        HFD.plot(lenses[i,1],
                 lenses[i,2],
                 col = cols[i],
                 ...,
                 add = i>1)
    }
    legend('bottomleft',
           legend = fl.str,
           adj=c(0,0.5),
           title = "f [mm]",
           pch='+',
           col = rev(cols),
           bg='white',
           box.col = NA)
    box()
}

