# f: focal length of lens        [mm]
# F: F-stop
# d: distance (set on lens)      [m]
# COC: circle of confusion       [mm]

# Depth of Field (DOF)
dof <- function(d,                   # [m]
                f   = 85,            # [mm]
                F   = 1.2,
                COC = 0.03,
                verbose = getOption('verbose')){         # [mm]
    H <- f^2/(F*COC)
    dmm <- d*1000
    n <- H*dmm
    dd <- dmm - f
    near <- n/(H+dd)
    far <- n/(H-dd)
    if(far<0) far <- Inf   # beyond hyperfocal distance, -> Inf
    if(verbose){
        cat(sprintf("%f - %f - %f\n",
                    round(near/1000,4),
                    round((far-near)/1000,4),
                    round(far/1000,4)))
    }
    return(c(near,far)/1000)
}

# near/far focus
get.nf <- function(f, F, min=1, max=100, COC = 0.03, ...){
    H <- HFD(f, F, COC)
    #x <- as.vector(outer(c(1,1.2,1.5,1.7,2,2.5,3,4,5,6,7,8,9), 10^seq(0, ceiling(log10(max)))))
    x <- min:max
    nf <- data.frame(x, t(sapply(x, FUN=dof, f=f, F=F, COC=COC)))
    if(max>H/1000){
        cat(sprintf("HFD(%dmm,%2g): %.2fm\n",f, F, H/1000))
    }
    dimnames(nf) <- list(x,
                         c('dist', 'near', 'far'))
    nf <- within(nf,
                 range <-  far-near)
    return(nf[nf[,1]<=max,])
}

get.all.nf <- function(f, F0, Fn=22, ...){
    #Fseq <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2, 2.5, 2.8, 3.5, 4, 4.5, 5, 5.6, 6.3, 7.1, 8,
    #)
    Fseq <- c(1.0, 1.4,  2,  2.8, 4, 5.6, 8, 11, 16, 22, 32, 44, 64)
    Fs <- unique(sort(c(F0, Fn, Fseq)))
    Fs <- Fs[Fs >= F0 & Fs <= Fn]
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

plot.nf <- function(nf, f=NULL, add = FALSE, ...){
    if (!add){
        plot(c(0, max(nf[,1])),
             c(0, min(max(nf[,3]), 4*max(nf[,1]) )),
             type = 'n',
             xlab = 'Focus Distance',
             ylab = 'Focus Range',
             main = sprintf('Near and far focusing limits [f=%dmm]', f))
        lines(nf[,1], nf[,1], lty = 2, col  = 'lightgray')
    }
    lines(nf[,1], nf[,2], ...)
    lines(nf[,1], nf[,3], ...)
}

full.nf.plot <- function(f, F0, Fn=22, maxfr=NULL, ...){
    #Fseq <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2, 2.5, 2.8, 3.5, 4, 4.5, 5, 5.6, 6.3, 7.1, 8,
    #)
    Fseq <- c(1.0, 1.4,  2,  2.8, 4, 5.6, 8, 11, 16, 22, 32)
    Fs <- unique(sort(c(F0, Fn, Fseq)))
    Fs <- Fs[Fs >= F0 & Fs <= Fn]
    sq <- seq_along(Fs)
    cols <- rainbow(length(sq), end = 0.6)
    all.nf <- get.all.nf(f, F0, Fn=22, ...)

    new <- TRUE
    for (i in c(max(sq),sq)){
        plot.nf(all.nf[,,i], f=f, add = !new, ..., col = cols[i])
        if(new) grid()
        new <- FALSE
    }
    legend('topleft',
           legend = dimnames(all.nf)[[3]],
           title = "F",
           lwd = 1,
           col = cols,
           bg="#FFFFFFa0")
    return(invisible(all.nf))
}

f.values <- function(by=1){
    Fseq <- c(1.0, 1.1,1.2, 1.4, 1.6, 1.8,
              2, 2.2,2.5, 2.8, 3.2,3.5,
              4, 4.5, 5, 5.6, 6.3, 7.1,
              8, 9.0, 10,11,13,14,
              16, 18,20,22,25,28,
              32,36,40,44,50,56,
              64)
    return(Fseq[seq(1,length(Fseq), by=by)])
}

# Hyperfocal distance
#
HFD <- function(f, F, COC = 0.03){
    return(f^2/(F*COC))  # [mm]!
}

HFD.plot <- function(f,
                     F0  = 1,
                     COC = 0.03,
                     add = FALSE,
                     ...){
    Fseq <- c(1.0, 1.1,1.2, 1.4, 1.6, 1.8, 2, 2.2,2.5, 2.8, 3.2,3.5,
              4, 4.5, 5, 5.6, 6.3, 7.1, 8,
              9.0, 10,11,13,14,16,18,20,22,25,28,32)
    pc <- rep(c('+','.','.'), length = length(Fseq))
    use <- Fseq>=F0
    useF <- Fseq[use]
    H <- HFD(f, useF, COC)

    if(add){
        points(useF, H/1000,
               pch = pc[use],
               cex = 2,
               ...)
    } else {
        plot(useF, H/1000, log='xy',
             pch = pc[use],
             cex = 2,
             xlim = range(Fseq),
             ylim = c(1, max(H)/1000),
             main = 'Hyperfocal Distance',
             xlab = 'Aperture',
             ylab = 'HD [m]',
             ...)
    }
}

Hyperfocal.plot <- function(...){
    lenses <- cbind(f=c(400,200,100,85,70,50,24,17,8),
                    F=c(5.6,2.8,4,1.2,2.8,1.4,2.8,4,4))
    n <- nrow(lenses)
    cols <- rainbow(n, end = 0.6)
    for(i in 1:n){
        HFD.plot(lenses[i,1], lenses[i,2], col = cols[i], ..., add = i>1)
    }
    abline(h=as.vector(outer(c(1,2,5), 10^seq(0, ceiling(log10(par('usr')[4]))))),
           col = "lightgray",
           lty = "dotted",
           lwd = par("lwd"))
    abline(v=f.values(6),
           col = "lightgray",
           lty = "dotted",
           lwd = par("lwd"))
    legend('bottomleft', legend = rev(lenses[,1]), pch='+', col = rev(cols),
           bg='white', box.col = NA)
    box()
}

