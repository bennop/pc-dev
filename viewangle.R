viewangle.plot <- function(f = NULL,
                           long.short = c('l', 's', 'd', 'n'),
                           direction = c('horizontal', 'vertical', 'diagonal', 'none'),
                           crop = 1,
                           sub = TRUE,
                           ...){
    long.short <- match.arg(long.short)
    long.dir <- unique(match.arg(direction,
                                 several.ok = FALSE))
    direction <- substr(long.dir, 1, 1)
    ls.string <- c(s = 'short', l = 'long', d = 'diagonal', n = 'none')

    if (length(f)>1){
        f <- unique(sort(f))
    }
    len.f <- length(f)

    d <- c(12, 18, sqrt(12^2 + 18^2))
    names(d) <- c('s', 'l', 'd')

    dist.rng <- 0:1*2       # in viewing direction
    view.rng <- c(-1,1)     # perpendicular to viewing direction

    for(fi in 1:len.f){
        # viewing angle
        phi <- atan2(d, f[fi] * crop)   # this is half the view angle, i.e., the
                                        # angle between central line and edge of
                                        # field of view
        phi.deg <- (phi)*180/pi

        # annotation strings for plot
        main.str <- paste(long.dir,
                          ifelse(direction != 'd',
                                 paste0(" (",
                                       ls.string[long.short],
                                       " edge)"),
                                 ""))
        sub.str <- paste0(f[fi], "mm",
                          ifelse(crop != 1,
                                 paste0(" (crop: ",
                                        round(crop,1),
                                        ")"),
                                 ""))

        if(long.short == 'n'){
            # produce empty plot
            plot(1,
                 type = 'n',
                 axes = FALSE,
                 xlab = '',
                 ylab = '')
        }  else {
            dir.v <- direction == 'v'    # abbreviate
            if(fi == 1){
                plot(1,
                     type = 'n',
                     asp  = 1,
                     pty  = 's',
                     xaxs = 'i',
                     yaxs = 'i',
                     axes =  FALSE,
                     xlim = if(dir.v) dist.rng else view.rng,
                     ylim = if(dir.v) view.rng else dist.rng,
                     xlab = '',
                     ylab = ''
                     )

                # graphics::box();
                grid()
                title(main = main.str,
                      sub  = ifelse(sub, sub.str, ""))
                eval(parse(text = paste0("abline(",
                                        ifelse(dir.v, 'h', 'v'),
                                        " = 0, col = 'grey')")))
            }
            mtext(paste0(round(2*(phi.deg[long.short]),1), "°"),
                  ifelse(dir.v, 4, 3),
                  at = 0,
                  line = 1 - fi,
                  cex = 0.7,
                  las = 1 # horizontal
            )
            
            slope <- ifelse(dir.v, d[long.short] / f[fi], f[fi] / d[long.short])

            # message("𝜑: ", phi[long.short] ,' -> ' ,phi.deg[long.short])
            abline(0,  slope)
            abline(0, -slope)


            max.h <- 1  # ???
                                        #print(par('usr'))

                                        # show width at 1m distance
            p <- tan(phi[long.short])
            width <- 2*p
            unit <- "m"
            if(width < 1){
                width <- width*100
                unit <- "cm"
            }
            if(dir.v){
                 lines(c(1,1), c(-p,p),
                      col = 'red')

                text(1, 0,
                     paste(round(width, log10(width)<1), unit),
                     pos = ifelse(len.f == 2 && fi == 1, 2, 4) )
                text(0.5, 0,
                     "1m",
                     col = 'grey',
                     #srt = 90,
                     pos = 3)
           } else {
                lines(c(-p,p), c(1,1),
                      col = 'red')

                text(0, 1,
                     paste(round(width, (log10(width)<1)+1), unit),
                     pos = ifelse(len.f == 2 && fi == 1, 1, 3))
                text(0, 0.5,
                     "1m",
                     col = 'grey',
                     srt = 90,
                     pos = 3)
           }
            if(dir.v){
                # cover left of origin (square plot not observed in shiny?)
                rect(-2,-2,-0.01,2,
                     col = 'white',
                     border = NA)
            }
        }
    }
}
