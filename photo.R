dof <- function(d,
                f   = 85,
                F   = 1.2,
                COC = 0.03){
    H <- f^2/(F*COC)
    dmm <- d*1000
    n <- H*dmm
    dd <- dmm - f
    near <- n/(H+dd)
    far <- n/(H-dd)
    if(far<0) far <- 1e8
    sprintf("%f - %f - %f\n", round(near/1000,4), round((far-near)/1000,4), round(far/1000,4))
    return(c(near,far)/1000)
}

get.nf <- function(f, F, min=1, max=100, COC = 0.03, ...){
    H <- f^2/(F*COC)
    x <- as.vector(outer(c(1,1.2,1.5,1.7,2,2.5,3,4,5,6,7,8,9), 10^seq(0, ceiling(log10(max)))))
    nf <- rbind(x,sapply(x, FUN=dof, f=f, F=F, COC=COC))
    return(nf[,nf[1,]<=max])
}

plot.nf <- function(nf, add = FALSE, ...){
    if (!add){
        plot(c(0,max(nf[1,])), c(0, max(nf[3,])),
             type = 'n',
             xlab='Distance', ylab='Focus',
             xlim = c(0, max(nf[1,])))
        lines(nf[1,], nf[1,], lty = 2)
    }
    lines(nf[1,], nf[2,], ...)
    lines(nf[1,], nf[3,], ...)
}

full.nf.plot <- function(f, F0, Fn, ...){
    #Fseq <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2, 2.5, 2.8, 3.5, 4, 4.5, 5, 5.6, 6.3, 7.1, 8,
    #)
    Fseq <- c(1.0, 1.4,  2,  2.8, 4, 5.6, 8, 11, 16, 22, 32)
    Fs <- sort(c(F0, Fn, Fseq))
    Fs <- Fs[Fs >= F0 & Fs <= Fn]
    sq <- seq_along(Fs)
    cols <- rainbow(length(sq), end = 0.6)
    for (i in sq){
        cat(Fs[i],'\n')
        nf <- get.nf(f, Fs[i], ...)
        plot.nf(nf, add = i>1, ..., col = cols[i])
    }

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
    H <- f^2/(useF*COC)

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
    lenses <- cbind(f=c(400,200,100,85,70,50,24,17),
                    F=c(5.6,2.8,4,1.2,2.8,2.5,2.8,4))
    n <- nrow(lenses)
    cols <- rainbow(n, end = 0.6)
    for(i in 1:n){
        HFD.plot(lenses[i,1], lenses[i,2], col = cols[i], ..., add = i>1)
    }
    grid()
    legend('bottomleft', legend = rev(lenses[,1]), pch='+', col = rev(cols),
           bg='white', box.col = NA)
    box()
}

