 ## f: focal length of lens        [mm]
## F: F-stop
## d: distance (set on lens)      [m]
## COC: circle of confusion       [mm]


##' Simple knit and typeset
##'
##' simplify typesetting process outside of RStudio
##'
##' CAVEAT: if there is a problem in runnning \code{latexcmd}, it is necessary to kill the command from an external shell to regain control on the Console command line.
##' @title go
##' @return none
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
##' @param n number of LaTeX runs
##' @param bib run BibTeX?                 (not yet implemented)
##' @param index create index?             (not yet implemented)
##' @param indexcmd command to create index file
##' @param latexcmd command to invoke LaTeX
##' @param bibcmd   command to create bib file
##' @param ltx.opts other options to LaTeX
##' @param base basename of project files
go <- function(n        = 1,
               bib      = FALSE,
               index    = FALSE,
               latexcmd = 'pdflatex',
               bibcmd   = 'bibtex',
               indexcmd = 'makeidx',
               base     = 'photo',
               ltx.opts = ''){
    require (knitr)
    latex <- function(){
        system(paste(latexcmd, ltx.opts, base))
    }
    od <- setwd('~/Work/git/Photo')
    on.exit(setwd(od))

    knit(paste0(base,'.Rnw'))
    latex()

    if(bib){
        system(paste(bibcmd, base))
        latex()
    }

    if(index){
        system(paste(indexcmd, base))
        latex()
    }

    if(!(bib | index)){
        for (i in 1:(n-1)){
            system(paste('pdflatex', base))
        }
    }
}

#
#
##' Standard F stops
##'
##' F-stops in steps of 1/3, setting \code{by} to 3 or 6 gives steps
##' of 1 (i.e., half the light) or 1/2 (one quarter per step).
##'
##' Starting and final aperture are inserted if they are not part of the requested series.
##' ##' @title f.values
##' @param from maximum (starting) aperture
##' @param to minimum aperture
##' @param by stride  in sequence to return, see details
##' @param ... not used
##' @return vector of F-stops
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
#'
#' @examples
#' f.values()
#' f.values(2.8, 11, by = 6)
f.values <- function(from = 1.0,
                     to   = 22,
                     by   =  1,
                     ...){   
    if(length(from)==2){
        to <- max(from)
        from <- min(from)
    }
    Fseq <- c( 1.0,  1.1,  1.2,  1.4,  1.6,  1.8,
               2.0,  2.2,  2.5,  2.8,  3.2,  3.5,
               4.0,  4.5,  5.0,  5.6,  6.3,  7.1,
               8.0,  9.0, 10.0, 11.0, 13.0, 14.0,
              16.0, 18.0, 20.0, 22.0, 25.0, 28.0,
              32.0, 36.0, 40.0, 44.0, 50.0, 56.0,
              64.0)
    fs.by <- Fseq[seq(1, to = length(Fseq), by = by)]

    fss <- unique(sort(c(from, to, fs.by))) # adding sort allows switching \code{from} and \code{to}

    return(fss[fss >= from & fss <= to])
}



##' Depth of Field (DOF)
##'
##' CoC-based calculation of depth of field (diffraction ignored)
##' @title Depth of field
##' @param d focussing distance    [m]
##' @param f focal length of  lens [mm]
##' @param F aperture
##' @param COC circle of confusion [mm]
##' @param verbose if set print near-depth-far
##' @return two-element vector with near and far focussing limits [m]
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
#'
#' @examples
#' dof(1)           # 85mm w/ F/1.2 @ 1m
#' dof(3, 16, 11)   # 16mm w/ F/11  @ 3m
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
    far  <- ifelse(dd>H,
                   Inf,              # beyond hyperfocal distance, -> Inf
                   n/(H-dd))

    if(verbose){                     # feedback
        cat(sprintf("%f - %f - %f\n",
                    round(near,4),
                    round((far-near),4),
                    round(far,4)))
    }

    return(c(near,far))
}

#' image distance
#'
#' Distance of focus plane (sensor/film) behind lens
#'
#' @param g object distance [m]
#' @param f focal length [mm]
#' @param ... not used
#'
#' @return image distance [mm]
#' @export
#' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
#' @examples
#'
b <- function(g, f, ...){
    gmm <- g*1000
    return(gmm*f/(gmm-f))
}

#' magnification factor
#'
#' @param g object distance [m]
#' @param f focal length [mm]
#' @param ... not used
#'
#' @return image distance [mm]
#' @export
#' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
#' @examples
mag <- function(g, f, ...){
    # internally all im [mm]
    return(f/(g*1000-f))
}

#' Vector of default distances for \code{\link{get.nf}}
#'
#' @param min minimum distance       [m]  [[   1]]
#' @param max maximum distance       [m]  [[ 100]]
#' @param n number of points to calculate [[ 500]]
#' @param step desired step size (overrides \code{n} if provided)
#' @param ... ignored
#'
#' @return vector of distance values
#' @author Benno Pütz \email{puetz@@psych.mpg.de}
#'
#' @examples
#' dist.defaults()
dist.defaults <- function(min  =  0.25,
                          max  = 100,
                          n    = 500,
                          step = NULL,
                          verbose = getOption('verbose'),
                          ...){
    if(is.null(step)){
        step.options  <- outer(1/c(1,2,4,5), 10^(0:(-2)))
        rng <- max - min
        step <- step.options[which.min((rng/n - step.options)^2)]
    }
    #if (verbose)
        message(sprintf("%f:%f by %f\n", min, max, step))
    return(unique(round(c(min,
                          seq(min + step - min %% step,
                              max,
                              by = step),
                          max),
                        3)))
}

# near/far focus
#
##' Get near and far limits for a single aperture (given focal length and CoC)
##'
##' The limits are calculated for distances between \code{min} and \code{max}
##' and returned with range, before, after, and before/after ratio
##'
##' The near and far limits indicate object distances where the blurring of
##' point sources reaches \code{COC}
##'
##' CAVEAT: \code{f} and \code{F} are expected to be scalar (1-element vectors),
##' only their respective first element is used.
##' @title get near and far limits
##' @param f focal length of lens     [mm]
##' @param F aperture
##' @param min minimum distance       [m]  [[   1]]
##' @param max force maximum distance [m]  [[    ]]
##' @param COC circle of confusion    [mm] [[0.03]]
##' @param n number of points to calculate [[ 500]]
##' @param x.vec predefined vector for x
##' @param ... not used
##' @return dataframe with columns
##' \item{distance}{focus distance (d)}
##' \item{near}{near limit}
##' \item{far}{far limit}
##' \item{range}{distance between near and far (far-near), i.e., depth of field (DOF)}
##' \item{close}{DOF range in front of focus distance (d-near)}
##' \item{behind}{DOF range behind focus distance (far-d)}
##' \item{ratio}{ratio behind/close}
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
##' @examples
##' get.nf(50, 1.4)
get.nf <- function(f,
                   F,
                   min   = 0.5,
                   max   = NULL,
                   COC   = 0.03,
                   n     = 500,
                   x.vec = NULL,
                   quiet = FALSE,
                   verbose = getOption('verbose'),
                   ...){
    H <- HFD(f[1], F[1], COC)
    if(is.null(max)) max <- max(H)
    x <- if(is.null(x.vec)){
        if(min==max) {
            min
        } else {
            full.x <- dist.defaults(min, max, n = n)
            full.x[full.x >= min & full.x <= ceiling(max)]
        }
    } else {
        x.vec
    }
    nf <- data.frame(x,
                     t(sapply(x,
                              FUN = dof,
                              f   = f[1],
                              F   = F[1],
                              COC = COC)))
    if(max>H){
        # adjust output ?
        if(verbose){
            cat(sprintf("HFD(%dmm,%2g): %.2fm\n",f[1], F[1], H))
        }
    }
    dimnames(nf) <- list(x,
                         c('dist', 'near', 'far'))
    nf <- within(nf,
                 {   # added in reverse order
                     behind <- far - x
                     close  <- x - near
                     ratio  <- close/behind
                     range  <- far - near
                 }
                 )
    attr(nf, 'focal length') <- f[1]
    attr(nf, 'aperture') <- F[1]
    attr(nf, 'coc') <- COC
    return(nf)
}

##' Get near and far limits for a set of apertures (given focal length and CoC)
##'
##' Extended version of \code{\link{get.nf}} to handle multiple apertures.
##'
##' @title  Get near and far limits
##' @param f focal length of  lens [mm]
##' @param F0 starting (maximum) aperture (smallest F number)
##' @param Fn final (minimum) aperture (largest F number)
##' @param all.nf provide precomputed value
##' @param ... pass to \code{\link{get.all.nf}}
##' @return array with \code{nf}-type slices for
##' each aperture. The slice columns are (see \code{\link{get.nf}})
##' \item{distance}{focus distance (d)}
##' \item{near}{near limit}
##' \item{far}{far limit}
##' \item{range}{distance between near and far (far-near), i.e., depth of field (DOF)}
##' \item{close}{DOF range in front of focus distance (d-near)}
##' \item{behind}{DOF range behind focus distance (far-d)}
##' \item{ratio}{ratio behind/close}
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
##' @examples
##' get.all.nf(50, 1.4, 16)
get.all.nf <- function(f,
                       F0,
                       Fn = 22,
                       max = NULL,
                       ...){
    Fs <- f.values(F0, Fn, by = 3)
    sq <- seq_along(Fs)

    max.hfd <- HFD(f, min(Fs), ...)   # largest HFD for minmum aperture value

    dist.vec <- dist.defaults(max = ifelse(is.null(max),
                                           max.hfd,
                                           max),
                              ...)

    nf1 <- get.nf(f, F, ..., x.vec = dist.vec)      # needed for dimnames below

    all.nf <- sapply(Fs,
                     function(F) as.matrix(get.nf(f, F, ..., x.vec = dist.vec)),
                     simplify = 'array')
    dimnames(all.nf) <- list(distance = all.nf[,1,1],
                             focus    = colnames(nf1),
                             aperture = Fs)
    return(all.nf)
}

##' Plot near and far limits vs. focussing distance for
##' a single aperture and given focal length
##'
##' .. content for \details{} ..
##' @title Near/far plot
##'
##' @param nf
##' @param f focal length of  lens [mm]
##' @param add
##' @param y.ext
##' @param ...
##'
##' @return
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
##' @examples
##' nf <- get.nf(50, 1.4)
##' get.nf(nf, 50)
plot.nf <- function(nf,
                     f   = NULL,
                     F   = NULL,
                     col = 'red',
                     add = FALSE,
                     y.ext = 1,
                     log = '',
                     ...){
    need <- ''
    if(is.null(f)){
        f <- attr(nf, 'focal length')
        if(is.null(f)) need <- c(need, "f")
    }
    if(is.null(F)){
        F <- attr(nf, 'aperture')
        if(is.null(F)) need <- c(need, "F")

    }
    if(length(need)>1){
        stop("Please provide ", paste(need[-1], collapse = " and "), ".")
    }
    hfd <- HFD(f, F)
    if (!add){
        plot(1,
             type = 'n',                                  # empty plot to start with
             xlim = c(ifelse(grepl('x', log), min(nf[,1]),  0),
                      min(max.finite(nf[,1]),
                          max.finite(hfd))),    # na.rm = TRUE does not remove Inf values
             ylim = c(ifelse(grepl('y', log), min(nf[,2:3]),  0),
                      min(max.finite(unlist(nf[,2:3])) * y.ext,
                          max.finite(hfd),
                          4 * max.finite(nf[,1]))),
             xlab = 'Focus Distance [m]',
             ylab = 'Focus Range',
             main = sprintf('Near and far focusing limits [f=%dmm]', f),
             las  = 1,
             log  = log,
             ...)
        lines(nf[,1], nf[,1], lty = 2, col  = 'lightgray')    # "diagonal"
    }
    lines(nf[,1], nf[,2], col = col, ...)     # near
    lines(nf[,1], nf[,3], col = col, ...)     # far
}

##' Common basis for plots off the near-far (\code{nf}) dataframe obtained
##' through \code{\link{get.nf}}
##'
##' One or more columns of \code{nf} are plotted over the focus distance
##' (first column).
##'
##' Usually, i.\,e. when obtaining \code{nf} from  \code{\link{get.nf}},
##' focal length and aperture are included as attributes. In cases of other
##' origin, e.g., as a slice of  \code{\link{get.all.nf}}, these values should
##' be provided via the corresponding parameters. Otherwise the function aborts
##' with an error.
##' @title
##' @param nf from \code{\link{get.nf}}
##' @param plot.columns which columns of \code{nf} do plot
##' @param f focal length (needed when not provided as attrbiute to \code{nf})
##' @param F aperture     (likewise, see description)
##' @param col color
##' @param add whether to add to existing plot
##' @param y.ext
##' @param log log axes?
##' @param main pass to plot
##' @param ylab pass to plot
##' @param ...
##' @return none
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
nf.plot.base <- function(nf,
                         plot.columns = 4,
                         f   = NULL,
                         F   = NULL,
                         col = 'red',
                         add = FALSE,
                         y.ext = 1,
                         log = '',
                         main = paste(names(nf)[plot.columns],
                                     collapse=' '),
                         ylab = NULL,
                         diag = TRUE,
                         hfd  = TRUE,
                         ...){
    need <- ''
    if(is.null(f)){
        f <- attr(nf, 'focal length')
        if(is.null(f)) need <- c(need, "f")
    }
    if(is.null(F)){
        F <- attr(nf, 'aperture')
        if(is.null(F)) need <- c(need, "F")

    }
    if(length(need)>1){
        stop("Please provide ", paste(need[-1], collapse = " and "), ".")
    }
    HFD <- HFD(f, F)
    if (!add){
        plot(1,
             type = 'n',                                  # empty plot to start with
             xlim = c(ifelse(grepl('x', log), min(nf[,1]),  0),
                      min(max.finite(nf[,1]),   # na.rm = TRUE does not remove Inf values
                          HFD)),
             ylim = c(ifelse(grepl('y', log), min(nf[, plot.columns]),  0),
                      min(max.finite(unlist(nf[, plot.columns])),
                          4 * max.finite(nf[,1])) * y.ext),
             xlab = 'Focus Distance [m]',
             ylab = ylab,
             main = main,
             las  = 1,
             log  = log,
             ...)
        if(diag)
            lines(nf[,1], nf[,1], lty = 2, col  = 'lightgray')    # "diagonal"
    }
    for (i in plot.columns){
        lines(nf[,1], nf[,i], col = col, ...)
    }
    if(hfd)
        show.HFD(HFD,
                 col = col,
                 ...)
}

##' DOF-wrapper for \code{\link{nf.plot.base}}
##'
##' .. content for \details{} ..
##' @title
##' @param nf
##' @param f
##' @param ...
##' @return
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
dof2.plot <- function(nf,
                      f = NULL,
                      ...){
    nf.plot.base(nf,
                 f = f,
                 ...,
                 plot.columns = 4,
                 ylab = 'Depth [m]',
                 main = 'Depth of Field',
                 diag = FALSE)
}

##' Near/Far-wrapper for \code{\link{nf.plot.base}}
##'
##' .. content for \details{} ..
##' @title
##' @param nf
##' @param f
##' @param ...
##' @return
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
nf2.plot <-  function(nf,
                      f = NULL,
                      ...){
    nf.plot.base(nf,
                 f = f,
                 ...,
                 plot.columns = 2:3,
                 ylab = 'Focus Range [m]',
                 main = 'Near and far focusing limit')
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
##' @return \code{all.nf} (invisibly)
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
full.nf.plot <- function(...){
    ##    full.plot.base(plot.nf, ...)
    full.plot.base(nf2.plot,
                   plot.columns = 2:3,
                   ...)
    ## Fs <- f.values(F0, Fn, by=3)
    ## sq <- seq_along(Fs)
    ## cols <- rainbow(length(sq),
    ##                 end = 0.6)
    ## all.nf <- get.all.nf(f, F0, Fn=Fn, ...)

    ## new <- TRUE
    ## for (i in rev(sq)){   # start with max aperture?
    ##     plot.nf(all.nf[,,i],
    ##             f   = f,
    ##             add = !new,
    ##             ...,
    ##             col = cols[i],
    ##             hfd = FALSE)
    ##     if(new) {
    ##         grid()
    ##         pu <- par('usr')
    ##         new <- FALSE
    ##     }
    ##     H <- HFD(f, Fs[i], ...)
    ##     ## if(max(all.nf[,4,i])>ifelse(par('ylog'),10^pu[4], pu4[4])){

    ##     ## if(H < pu[2]){
    ##     ##     rug(H,
    ##     ##         ticksize = -0.02,
    ##     ##         side     = 3,
    ##     ##         col      = cols[i],
    ##     ##         lwd      = 1)
    ##     ##     abline(v   = H,
    ##     ##            col = adjustcolor(cols[i],
    ##     ##                              alpha.f = 0.4),
    ##     ##            lty = 3)
    ##     ## }
    ## }
    ## show.HFD(HFD(f, Fs, ...), ...)
    ## legend('topleft',
    ##        legend = dimnames(all.nf)[[3]],
    ##        title  = "F",
    ##        lwd    = 1,
    ##        col    = cols,
    ##        bg     = "#FFFFFFa0")
    ## attr(all.nf, 'focal length') <- f
    ## return(invisible(all.nf))
}

##' Base function for full.xxx.plot()
##'
##' Consolidate common functionality for \code{\link{full.dof.plot}}.
##' Not meant to be called directly by user
##' and  \code{\link{full.nf.plot}}
##' @title full.plot.base
##' @param plot.fun
##' @param f
##' @param F0
##' @param Fn
##' @param ...
##' @param maxfr
##' @return
##' @author Benno Pütz \email{puetz@@psych.mg.de}
full.plot.base <- function(plot.fun,
                           f,
                           F0,
                           Fn = 22,
                           log = '',
                           plot.columns = 1, # dummy = identity plot
                           y.ext = 1,
                           ylab = '',
                           main = 'Depth of Field',
                           ...,
                           maxfr = NULL){
    if(length(F0) == 2){ # Interpret as vector (F0, Fn)
        if(hasArg(Fn)){
            warning("Fn provided twice")
        }
        Fn <- F0[2]
        F0 <- F0[1]
    }
    Fs <- f.values(F0, Fn, by=3)
    sq <- seq_along(Fs)
    cols <- rainbow(length(sq),
                    end = 0.6)
    all.nf <- get.all.nf(f, F0, Fn=Fn, ...)

    new <- TRUE

    cat(sprintf(">%s<\n", log))
    ## set up plot with full axis ranges
    if(!hasArg(log)) {
        warning('no log param!\n')
        log  <- ""
    } else {
        print(str(log))
    }
    plot(1,
         type = 'n',                                  # empty plot to start with
         xlim = c(ifelse(grepl('x', log), all.nf[1],  0),
                  min(max(all.nf[,1,1]),   # na.rm = TRUE does not remove Inf values
                      HFD(f, F0, ...))),
         ylim = c(ifelse(grepl('y', log),
                         min(all.nf[, plot.columns, 1]),
                         0),
                  min(max.finite(unlist(all.nf[, plot.columns, 1])),
                      4 * max.finite(all.nf[,1,1])) * y.ext),
         xlab = 'Focus Distance [m]',
         ylab = ifelse(hasArg('ylab'), ylab, ""),
         main = main,
         las  = 1,
         ##log  = log,
         ...)
    for (i in sq){   # start with smallest aperture (= largest HFD)

        plot.fun(all.nf[,,i],
                 f   = f,
                 F   = Fs[i],
                 add = !new,
                 ...,
                 col = cols[i],
                 hfd = FALSE)
        if(new) {
            grid()
            pu <- par('usr')
            new <- FALSE
        }
        ## H <- HFD(f, Fs[i], ...)
        ## if(max(all.nf[,4,i])>ifelse(par('ylog'),10^pu[4], pu4[4])){

        ## if(H < pu[2]){
        ##     rug(H,
        ##         ticksize = -0.02,
        ##         side     = 3,
        ##         col      = cols[i],
        ##         lwd      = 1)
        ##     abline(v   = H,
        ##            col = adjustcolor(cols[i],
        ##                              alpha.f = 0.4),
        ##            lty = 3)
        ## }
    }
    show.HFD(HFD(f, Fs, ...), cols = cols, ...)
    legend('topleft',
           legend = dimnames(all.nf)[[3]],
           title  = "F",
           lwd    = 1,
           col    = cols,
           bg     = "#FFFFFFa0")
    attr(all.nf, 'focal length') <- f
    return(invisible(all.nf))
}

#' close/behind plot
#'
#' This is essentially a \code{\link{nf.plot}}
#' rotated by 45 degrees so that the focus line is
#' horizontal.
#'
#' @param nf
#' @param add add to existing plot?
#' @param max.d maximum distance [max(xlim)]
#' @param ... pass to \code{lines}
#'
#' @return
#' @export
#' @author Benno Pütz \email{puetz@@psych.mpg.de}
#'
#' @examples
cb.plot <- function(nf, add = FALSE, max.d = 1e4, ...){
    if(!add){
        plot(0:1, 0:1,
             xlim = c(0, max(nf[, 'dist'])),
             ylim = c(-max(nf[, 'close']),
                      min(max(nf[is.finite(nf[,'behind']), 'behind']),  max.d)),
             type = 'n',
             main = 'Before / behind',
             xlab = 'Distance  [m]',
             ylab = ' Focus spread [m]')
        lines(range(nf[, 'dist']),
              rep(0, 2),
              lty = 3,
              col = 'lightgrey')
    }
    lines(nf[, 'dist'],  nf[, 'behind'],
          ...)
    lines(nf[, 'dist'], -nf[, 'close'],
          ...)

}

#' Ratio plot
#'
#' show before/behind ratio
#' @param nf
#' @param add
#' @param ...
#'
#' @return none
#' @export
#' @author Benno Pütz \email{puetz@@psych.mpg.de}
#'
#' @examples
ratio.plot <- function(nf, add = FALSE,  ...){
    if(!add){
        plot(0:1, 0:1,
             xlim = c(0, max(nf[, 'dist'])),
             type = 'n',
             main = 'Before / behind - Ratio',
             xlab = 'Distance  [m]',
             ylab = 'Ratio',
             las  = 1)
        abline(h = 0:10/10,
              lty = 3,
              col = 'lightgrey')
    }
    finite.rat <- is.finite(nf[, 'ratio'])
    lines(nf[finite.rat, 'dist'],  nf[finite.rat, 'ratio'],
          ...)

}

## DOF plots
##
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Single
##' @param nf near.far (from \code{\link{get.nf}})
##' @param f focal length of  lens [mm]
##' @param add add to  existing plot? Otherwise set up plot
##' @param y.ext y extension
##' @param ... passed to \code{plot} and \code{\link{HFD}}
##' @return none
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
##' @examples
##' dof.plot(get.nf(50, 1.4)
dof.plot <- function(nf,
                     f   = NULL,
                     F   = NULL,
                     col = 'red',
                     add = FALSE,
                     y.ext = 1,
                     log = '',
                     ...){
    need <- ''
    if(is.null(f)){
        f <- attr(nf, 'focal length')
        if(is.null(f)) need <- c(need, "f")
    }
    if(is.null(F)){
        F <- attr(nf, 'focal length')
        if(is.null(F)) need <- c(need, "F")

    }
    if(length(need)>1){
        stop("Please provide ", paste(need[-1], collapse = " and "), ".")
    }
    hfd <- HFD(f,
               F,
               ...)
    if (!add){
        plot(1,
             type = 'n',        # empty plot
             xlim = c(ifelse(grepl('x', log), min(nf[,1]),  0),
                      min(max(nf[,1]),
                          hfd)),
             ylim = c(ifelse(grepl('y', log), min(nf[,2:3]),  0),
                      min(max.finite(unlist(nf[,2:3])) * y.ext,
                          max.finite(hfd),  # na.rm = TRUE does not remove Inf values
                          4 * max.finite(nf[,1]))),
             xlab = 'Focus Distance [m]',
             ylab = 'Depth [m]',
             main = sprintf('Depth of Field [f=%dmm]', f),
             las  = 1,
             log  = log,
             ...)
        grid()
    }
    lines(nf[,1], nf[,4], col = col, ...)
}

##' DOF plot for all f stops
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
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
full.dof.plot <- function(#f,
                          #F0,
                          #n    = 22,
                          #maxfr = NULL,
                          #y.ext = 1,
                          ...){
    full.plot.base(dof.plot,
                   plot.columns = 4,
                   ...)

    # Fs <- f.values(F0, Fn, by = 3)      # in full stops
    # sq <- seq_along(Fs)
    # cols <- rainbow(length(sq), end = 0.6)
    # all.nf <- get.all.nf(f, F0, Fn, ...)
    #
    # new <- TRUE
    # for (i in sq){
    #     dof.plot(all.nf[,,i],
    #              f     = f,
    #              add   = !new,
    #              y.ext = y.ext,
    #              ...,
    #              col   = cols[i],
    #              hfd = FALSE)
    #     if(new) {
    #         grid( )
    #         pu4 <- par('usr')[4]
    #         new <- FALSE
    #     }
    #
    #     ## if(max(all.nf[,4,i])>ifelse(par('ylog'),10^pu4, pu4)){
    #
    #     ## add mark on upper edge to indicate HFD
    #     # H <- HFD(f, Fs[i], ...)
    #     # rug(H,
    #     #     ticksize = -0.02,
    #     #     side     = 3,
    #     #     col      = cols[i],
    #     #     lwd      = 1)
    #     # abline(v   = H,
    #     #        col = adjustcolor(cols[i],
    #     #                          alpha.f = 0.4),
    #     #        lty = 3)
    #     #}
    # }
    # show.HFD(HFD(f, Fs, ...), ...)
    # legend('topleft',
    #        legend = dimnames(all.nf)[[3]],
    #        title  = "F",
    #        lwd    = 1,
    #        col    = cols,
    #        bg     = "#FFFFFFa0")        # semitransparent white
    # return(invisible(all.nf))
}

#' Mark hyperfocal distance(s)
#'
#' internal function to be called from various plot functions
#' @param hfd HFD (vector)
#' @param rug whether to show \code{rug}
#' @param line whether to show vertical line(s)
#' @param cols colors
#' @param width.rug width for rug lines
#' @param lty type for vertical lines
#' @param alpha transparency for vertical lines
#' @param ticksize length of rug lines
#' @param ... ignored
#'
#' @return none
#'
#' @examples
show.HFD <- function(hfd,
                     rug = TRUE,
                     line = TRUE,
                     cols      = rainbow(length(hfd)),
                     width.rug =  1,
                     lty       =  3,
                     alpha     =  0.4,
                     ticksize  = -0.02,
                     ...){
    for (i in seq_along(hfd)){
        if(rug){
            rug(hfd[i],
                ticksize = ticksize,
                side     = 3,
                col      = cols[i],
                lwd      = width.rug)
        }
        if(line){
            abline(v   = hfd[i],
                   col = adjustcolor(cols[i],
                                     alpha.f = alpha),
                   lty = 3)
        }
    }
}

## Hyperfocal distance
##
##' Calculate the hyperfocal distance
##'
##' For a given focal length  and aperture the
##' hyperfocal distance is the distance where
##' the far focussing limit reaches \eqn{\infty}{infinity}.
##' In other words, it is the shortest focussing
##' distance where infinitely far objects appear
##' focussed. The corresponding near focussing
##' limit is HFD/2.
##'
##' Both focal length (\code{f}) and aperture (\code{F})
##' may be vectors. The return value will have appropriate
##' dimension(s).
##' @title Hyperfocal distance
##' @param f focal length of lens                [mm]
##' @param F aperture
##' @param COC  acceptable circle of confusion   [mm]
##' @param ...
##' @return HFD in [m]
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
##'
##' @examples
##' HFD(50, 8)
##' HFD(c(24,50,85,135), 8)
##' HFD(50, f.values(2.8,11,by=3))
##' HFD(c(24,50,85,135), f.values(2.8,11,by=3))
HFD <- function(f,
                F,
                COC = 0.03,
                ...){
    if (length(f)>1 && length(F)>1){
        ### hfd <- outer(f, F, HFD)   # bad recursion
        hfd <- sapply(f,
                      function(fl) HFD(fl, F=F, COC=COC))
        dimnames(hfd) <- list(aperture     = F,
                              focal.length = f)

    } else {
        hfd <- (f^2/(F*COC) + f)/1000   # convert to [m] !
    }
    return(hfd)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param f focal length of  lens [mm]
##' @param ... pass to \code{\link{f.values}}, \code{\link{points}}, \code{\link{plot}}
##' @return none
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
HFD.plot <- function(f,           # [mm]
                     F0  = 1,
                     COC = 0.03,  # [mm]
                     add = FALSE,
                     ...){
    Fseq <- f.values(1, ...)            # start at 1 to have the marks consistent
    pc <- rep(c('+','.','.'),
              length.out = length(Fseq))
    use <- Fseq >= F0                     # subset here
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
##' @author Benno Pütz \email{puetz@@psych.mpg.de}
Hyperfocal.plot <- function(...){
    ## list of focal lengths (f) and corresponding maximum apertures (F)
    ## for some interesting lenses (zoom lenses are treated as two
    ## lenses with shortest and longest focal length)
    ##
    ## this list could be factored out (similar to f.values) to be
    ## more flexible
    lenses <- cbind(f = c(400, 200, 100,  85,  70,  50,  24, 17, 11, 8),
                    F = c(5.6, 2.8,   4, 1.2, 2.8, 1.4, 2.8,  4,  4, 4))

    n <- nrow(lenses)                   # number of lenses
    f.lengths <- rev(lenses[,1])        # focal lengths
    nfl <- nchar(f.lengths)             #
    max.nfl <- max(nfl)                 # max. number of chars in f.len strings
    fills <- sapply(nfl,
                    function(i)substr('        ',
                                      1,
                                      2*(max.nfl - (i-1)))) # two SPACEs have width of one digit
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

# DOF calc comparison?
ddd <- function(d, f, F, COC = 0.03){
    dmm <- d*1000
    nf <- get.nf(f, F, d, d, COC)
    H <- HFD(f, F, COC)*1000
    dmf <- dmm-f
    hh <- (f^2/F/COC)
    my.dof1 <- (2*H*dmm *dmf / (H^2 -dmf^2))/1000
    my.dof2 <- (H*dmm*(1/(H-dmf) - 1/(H+dmf)))/1e3
    my.dof3 <- hh*dmm*2*dmf/(hh^2 - dmf^2)/1000
    cat(((1-(my.dof3-my.dof2)/my.dof2)*100), '\n')
    return(c(nf$range, my.dof1, my.dof2, my.dof3))
}

portraits <- function(lenses = c(50,70,85,90,100, 135),
                      ...){
    for (f in lenses){
        cat(paste0('\\subsection{',f,'mm}\\label{sec:',f,'}\n'))
        cat(paste0("<<portrait",f,", echo=FALSE, fig.height=9, fig.cap = paste0(",f,",'mm'), warning=FALSE,results='hide'>>=\n"))
        cat(paste0('full.dof.plot(',f,', 1.2, xlim=c(1,3), , ylim = c(0.01,10),, log="xy",  y.e=15)\n'))
        cat('@\n\n')
    }
}

#' viewing angle
#'
#' @param f focal length
#' @param d dimension of sensor
#' @param cropcrop factor
#'
#' @return viewing angle
#' @export
#'
#' @examples
va <- function(f, d = 43.2, crop = 1){
    return(2*atan2(d/2/crop, f)*180/pi)
}

#' modified modulo
#'
#' similar to "modulo" (%%) but returns n where `%%` returns 0, useful for indexing with recycled vectors
#'
#' @param e1 vector of numbers
#' @param e2 modulus
#'
#' @return
#' @export
#'
#' @examples
`%mymod%` <- function(e1, e2) {
    return(((e1-1) %%  e2) + 1)
}

#' unique-sort
#'
#' Return a sorted vector of the unique values in the input vector
#' @param vector a vector or something that can be coerced to a vector
#' @param ... passed to \code{sort}
#'
#' @return sorted vector of the unique values in the input vector
#' @export
#'
#' @examples
#' u.s(c(1:3, 8:3, 6:9))
u.s <- function(vector, ...){
    return(unique(sort(as.vector(vector), ...)))
}

#' find maximum finite value in \code{x}
#'
#' Similar to \code{na.rm = TRUE} for \code{Inf} values
#'
#' @param x numeric value (vector, matrix, array, ...)
#' @param ... passed to \code{max} (\code{na.rm = ...})
#'
#' @return maximum value, resstricted to finite elements
#' @export
#'
#' @examples
#'
max.finite <- function(x, ...){
    isfinite.x <- is.finite(x)
    if(sum(isfinite.x)){
        return(max(x[isfinite.x], ...))
    } else {
        return(NA)
    }
}

#' Add diagonal
#'
#' given horizontal and vertical dimensions of a sensor,
#' return a vector with those two plus the length of the
#' diagonal.
#' @param h horizontal
#' @param v vertical
#'
#' @return c(d, v, h)
#' @export
#'
#' @examples
#' add.diag(3,4)    # c(5,4,3)
add.diag <- function(h, v, decreasing = TRUE){
    return(sort(c(h,
                  v,
                  sqrt(h^2+v^2)),
                decreasing = decreasing))
}


#' axis transformation
#'
#' transformation to internal value inside plot to
#' allow for linear fit below
#' @param x value
#' @param axis axis along which value is shown,
#' either numeric (1...4) or as character ('x' or 'y')
#'
#' @return
#' @export
#'
#' @examples
ax.trafo <- function(x, axis){
    if(is.numeric(axis)){
        axis <-  ifelse(axis %% 2 == 1,
                        'x',   # 1 or 3
                        'y')   # 2 or 4
    }
    if(par(paste0(axis,'log'))){
        return(log10(x))
    } else {
        return(x)
    }
}

x.trafo <- function(x) ax.trafo(x, 'x')
y.trafo <- function(x) ax.trafo(x, 'y')


#' View angle plot
#'
#' @param d
#' @param crop
#' @param fs
#' @param base.color
#' @param dot.color
#' @param fit.color
#' @param ...
#'
#' @return none
#' @export
#'
#' @examples
#' vaplot(d=add.diag(36,24),crop=c(1,1.6),log='xy')
vaplot <- function(d = 43.2,
                   crop = 1,
                   fs = c(11,15,24,35,50,70,100,200,400,800),
                   base.color = 'black',
                   dot.color = 'red',
                   fit.color = adjustcolor(base.color, 0.5),
                   ...){
    n <- length(d)
    dc <- within(expand.grid(d=d, crop=crop), {
        d.eff <- d/crop
        leg.string <- ifelse(crop==1,'', paste0('   (',round(d, 1),':',crop,')'))
        })
    f.min <- ifelse(is.null(fs),  10,low  <- 10^floor  (log10(min(fs))))
    f.max <- ifelse(is.null(fs),1000,high <- 10^ceiling(log10(max(fs))))
    phi.range <- range(outer(c(f.min, f.max),
                             range(dc$d.eff),
                             function(f,d) va(f,d)))
    curve(va(x, d = dc[1, 'd.eff']),
          from = f.min,
          to   = f.max,
          las  = 1,
          xlab = 'Focal Length [mm]',
          ylab = 'Viewing Angle',
          axes = FALSE,
          ylim = phi.range,
          col  = base.color,
          ...)
    axis(1)
    l.angles = c(5, 10, 20, 30, 45, 60, 90, 120)
    axis(2,
         at = l.angles,
         labels = l.angles,
         las = 1)
    box()
    if (!is.null(fs) && nrow(dc)==1){
        points(fs, va(fs, d = dc[1, 'd.eff']),
               pch = 16,
               cex = 0.75,
               col = 'red')
    }
    # linear fit
    if(loglog <- (par('xlog') && par('ylog'))){
        do.linfit <- TRUE
        top.fs <- rev(rev(fs)[1:2])
        fit <- lm(va~f,
                  data.frame(f  = x.trafo(top.fs),
                             va = y.trafo(va(top.fs, d = dc$d.eff[1]))))
        abline(fit, col = adjustcolor(base.color, 0.25))
        curve(va(x, d = dc[1, 'd.eff']),
              col = base.color,
              add = TRUE)
    } else {
        do.linfit <- FALSE
    }
    #  grid
    abline(h = l.angles,
           lty = 3,
           col = 'lightgrey')
    abline(v = if(par('xlog')){
        if(is.null(fs)){
            sort(outer(c(1,2,5),10^(low:high)))
        } else {
            fs
        }} else {
            axTicks(1)
        },
        lty = 3,
        col = 'lightgrey')
    # legend
    legend(ifelse(loglog, 'bottomleft', 'topright'),
           legend = sprintf("%.1f %s",
                            dc[,'d.eff'],
                            dc[,'leg.string']),
           title  = 'Sensor size',
           cex    = 0.8,
           lty    = 1:n,
           col    = ifelse(dc[,2] == 1, 'black', 'grey'))
    # additional lines
    if(nrow(dc)>1){
        for (i in 2:nrow(dc)){
            line.col <- ifelse(dc[i,2] == 1, base.color, adjustcolor(base.color, 0.5))
            if(do.linfit){
                fit <- lm(va~f,
                          data.frame(f  = x.trafo(top.fs),
                                     va = y.trafo(va(top.fs, d = dc$d.eff[i]))))
                abline(fit,
                       col = adjustcolor(line.col, 0.25),
                       lty = i %mymod% n)
            }
            curve(va(x, d = dc[i, 'd.eff']),
                  add = TRUE,
                  col = line.col,
                  lty = i %mymod% n)
            # if (!is.null(fs)){
            #     points(fs,
            #            va(fs, d = dc[i,1], crop = dc[i,2]),
            #            pch = 16,
            #            cex = 0.75,
            #            col = 'red')
            # }
        }
    }
}

#' Title
#'
#' @param d
#' @param fs
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
h.angle.plot <- function(d  = 36,
                         fs = c(11,15,24,35,50,70,100,200,400,800),
                         ...){
    n <- length(fs)
    f.colors <- rainbow(n, end = 0.66)
    names(f.colors) <- fs
    plot(c(-1,1)*20,c(0,20), ty='n',
         yaxs = 'i', ylim=c(0, 20),
         xlab = 'Distance from optical axis [m]',
         ylab = 'Distance from Lens [m]',
         main = ifelse(d %in% (sen.sizes <- c(outer(2:3*12,c(1,1/1.6)), 14.9, 22.3)),
                       paste0(ifelse(d %in% c(24,36),
                                     "Full-format",
                                     "APS-C"),
                              ", ",
                              ifelse(portrait <- (d %in% sen.sizes[c(1,3,5)]),
                                     "short",
                                     "long"),
                              " sensor side (",
                              ifelse(portrait,
                                     'portrait',
                                     'landscape'),
                              ')'),
                       paste0("Sensor edge: ", d, "mm")),
         las  = 1,
         asp  = 1,        # fix aspect ratio to yield proper relations
         ...)
    grid()
    abline(v=0, lty=2, col = 'grey')
    abline(h=0, lty=2, col = 'grey')

    for  (f in fs){
        m <- f / (d/2)
        #cat(m, m*180/pi, "\n")
        abline(0,  m, col = f.colors[as.character(f)])
        abline(0, -m, col = f.colors[as.character(f)])
    }
    legend(par('usr')[2], 0,
           xjust  = 1,
           yjust  = 0,
           legend = fs,
           cex    = 0.6,
           lwd    = 1,
           col    = f.colors,
           title  = expression(f),
           bg     = "#FFFFFFB0")
}
