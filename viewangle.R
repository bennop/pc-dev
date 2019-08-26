viewangle.plot <- function(f=NULL,
                           orientation = NULL,
                           ...){
    possible.orientations <- c('horizontal', 'vertical', 'diagonal')
    if(is.null(orientation)){
        orientation <- possible.orientations
    } else {
        orientation <- substr(unique(match.arg(orientation,
                                               possible.orientations,
                                               several.ok = TRUE)),
                              1, 1)
    }
    d <- c(12, 18, sqrt(12^2 + 18^2))
    names(d) <- c('v', 'h', 'd')
    for(fi in  f){
        phi <- atan2(fi, d)
        for (o in orientation){
            cat(o)
            switch(orientation,
                   v = {
                       if(fi == f[1]){
                           plot(0:1*2, c(-1,1),
                                type = 'n',
                                pty = 's',
                                xaxs = 'i',
                                axes =  FALSE,
                                xlab = '', ylab = '',
                                main = 'vertical')
                           graphics::box()
                           abline(h = 0, col='grey')
                       }
                       phi.v <- -1/(fi/d[o])
                       message(phi[o], '->', phi.v*180/(pi/2))
                       abline(0,  phi.v)
                       abline(0, -phi.v)

                   },
                   d =,
                   h = {
                       if(fi == f[1]){
                           plot(c(-1,1), 0:1*2,
                                type = 'n',
                                pty = 's',
                                yaxs = 'i',
                                axes =  FALSE,
                                xlab = '', ylab = '',
                                main = ifelse(o='h', 'horizontal', 'diagonal'))
                           graphics::box()

                           abline(v = 0, col='grey')
                       }
                       phi.h <- (fi/d[o])
                       message(phi[o], '->', phi.h*180/(pi/2))

                       abline(0,  phi.h)
                       abline(0, -phi.h)

                   }
            )
        }
    }
}
