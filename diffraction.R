# Confucion circles

#' Convert meter to millimeter
#'
#' @param distance Value in meter
#'
#' @return equivalent in millimeters
#' @export
#'
#' @examples
m2mm <- function(distance){
  return(1000*m)
}

#' Geometric circle of confusion
#'
#' Calculate the diameter of the circle of confusion (COC)
#' as a function of distance given the parameters focus distance,
#' focal length, and aperture derived from geometric considerations.
#'
#' Diffraction is ignored, see \code{\link{COC.comb}} for a combined treatment.
#'
#' @param x distance parameter       [ m]
#' @param d focus distance           [ m]
#' @param f focal length of lens     [mm]
#' @param F aperture
#' @param ... ignored
#'
#' @return Diameter of geometric COC [mm]
#' @export
#'
#' @examples
COC.geom <- function(x,d,f,F,...){
  dmm <- d*1000
  xmm <- x*1000
  return(f^2/F*abs(dmm-xmm)/(xmm*(dmm-f)))
}

#' Confusion caused by diffraction
#'
#' Returns the diameter of the Airy disc (first minimum) for the given aperture
#'
#' @param F aperture
#' @param lambda wavelength [nm]
#' @param ... ignored
#'
#' @return diameter im mm
#' @export
#'
#' @examples
COC.diff <- function(F, lambda = 460, ...){
  # lambda in nm (= 1e-6 mm)
  return(2.44 * (lambda * 1e-6) * F)
}

# curve(COC.comb(x,1000,50,16),700,1400,  col='red', add = TRUE)

#' Combined Circle of Confusion
#'
#' Consider the combined effects of geometry (from \code{\link{COC.geom}})
#' and diffraction (from \code{\link{COC.diff}}) on the blurring of an
#' acquired image
#'
#' @param x distance              [ m]
#' @param d focus distance        [ m]
#' @param f focal length          [mm]
#' @param F aperture
#' @param ... passed to \code{\link{COC.diff}}
#'
#' @return diameter of the effective COC [mm]
#' @export
#'
#' @examples
COC.comb <- function(x, d, f, F, ...){
  #return(1/sqrt(COC.geom(x, d, f, F)^(-2) + COC.diff(F)^(-2)))
  ## conversion to mm takes place in COC.geom()
  return(sqrt(COC.geom(x, d, f, F)^2 + COC.diff(F, ...)^2))
}

#' Near distance within focus
#'
#' Return minimum distance considered within focus
#' (i.e., where the resulting COC does not exceed the desired target \code{coc})
#' given the parameters focus distance, focal length, aperture, and target COC.
#'
#' CAVEAT: If the blurring by diffraction exceeds the target COC
#' the desired focussing goal cannot be achieved and \code{NA} is returned.
#'
#' @param d focus distance
#' @param f focal length
#' @param F aperture
#' @param coc target COC
#' @param ... passed to \code{\link{COC.diff}}
#'
#' @return minimum distance in focus [mm]
#' @export
#'
#' @examples
near.ok <- function(d, f, F,
                    coc = 0.03,
                    ...){
    nok <- sapply(F, function(myF)ifelse(coc > COC.diff(myF, ...),
                  uniroot(function(x)COC.comb(x, d, f, myF, ...) - coc,
                          interval = c(f/1000, d))$root,
                  NA))
    return(nok)
}

#' Far distance within focus
#'
#' Return maximum distance considered within focus
#' (i.e., where the resulting COC does not exceed the desired target \code{coc})
#' given the parameters focus distance, focal length, aperture, and target COC.
#'
#' CAVEAT: If the blurring by diffraction exceeds the target COC
#' the desired focussing goal cannot be achieved and \code{NA} is returned.
#'
#' All distances are assumed in mm.
#' @param d focus distance [ m]
#' @param f focal length   [mm]
#' @param F aperture may be a vector
#' @param coc target COC   [mm]
#' @param ... passed to \code{\link{COC.diff}}
#'
#' @return maximum distance in focus [m]
#' @export
#'
#' @examples
far.ok <- function(d, f, F,
                   coc = 0.03,
                   ...){
    almost.inf <- 1e9

    possible <- COC.diff(F, ...) < coc
    reach.inf <- COC.comb(almost.inf, d, f, F) < coc
    finite <- possible & !reach.inf
    fok <- rep(Inf, length(F))
    fok[!possible] <- NA
    if(any(finite)){   # don't go in here if there are no finite cases
      fok[finite] <- sapply(F[finite],
                            function(myF)uniroot(function(x)COC.comb(x, d, f, myF, ...) - coc,
                                                 interval = c(d, almost.inf))$root)
      #    function(myF)ifelse(coc > COC.diff(myF, ...),
      #                        uniroot(function(x)COC.comb(x, d, f, myF, ...) - coc,
      #                                interval = c(d, almost.inf))$root,
      #                        Inf))
    }
return(fok)
}

#' Depth of Field (DoF)
#'
#' Calculate the effective depth of field given the parameters
#'  focus distance, focal length, aperture, and target COC.
#'
#' @param d focus distance [mm]
#' @param f focal length   [mm]
#' @param F aperture (may be vector)
#' @param coc target COC
#' @param ... passed to \code{\link{COC.diff}}
#'
#' @return DOF im mm
#' @export
#'
#' @examples
dof.diff <- function(d, f, F, coc = 0.03){
  return(far.ok(d, f, F, coc) - near.ok(d, f, F, coc))

}

#' Plot DoF with Diffraction vs. Aperture
#'
#' @param d focus distance [ m]
#' @param f focal length   [mm]
#' @param F aperture - should be a vector
#' @param coc sensor's COC [mm] 
#'
#' @return none
#' @export
#'
#' @examples
ddvF.plot <- function(d, f, F = f.values(),  coc = 0.03){
  dofs <- sapply(F,
                 function(myF){return(dof.diff(d,f, myF, coc = coc))})
  
  ## calculate much finer grid for better plotting
  F.fine <- seq(min(F), max(F), length.out = 1000)
  dofs.fine <- sapply(F.fine,
                      function(myF){return(dof.diff(d,f, myF, coc = coc))})
  
  ## browser()
  
  ## good <- is.finite(dofs)
  ## plot(smooth.spline(F[good], dofs[good]),
  plot(F.fine, dofs.fine,
       col  = 'red',
       log  = 'x',
       type = 'l',
       xlim = range(F),
       ylim = c(0, min (100, max(dofs, na.rm = TRUE))),
       main = 'Depth of field vs Aperture',
       xlab = 'Aperture',
       ylab = 'DOF [m]')
  points(F, dofs, pch = '+', cex = 0.65, col = 'darkred')
  grid()
  if(HFD(f,max(F),coc) < d){
    limitF <- uniroot(function(x)HFD(f, x, COC = coc) - d,
                      interval = range(F))$root
    message("Hfd < ", d, "m @ F", limitF, "\n")
    abline(v = limitF, lty = 2, col = 'red')
  } else {
    # HFD beyond d
  }
  
  ## browser()
  
  if (any(is.na(dofs))){     # diffraction limited
    pu <- par('usr')
    rect(F[which.max(is.na(dofs))], pu[3], 10^pu[2], pu[4],
         col = grey(0.920), border = NA)
  }

}

#' Depth-of-field plot based on CoC calculation with diffraction
#'
#' @param d focus distance [mm]
#' @param f focal length   [mm]
#' @param F aperture
#' @param near near plotting limit
#' @param far far plotting limit
#' @param coc accepted CoC (sensor [mm])
#' @param cmax
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
DOFcoc.plot <- function(d, f, F,
                        near = d/2,
                        far  = 2*d,
                        coc  = 0.03,
                        cmax = NULL,
                        geom = TRUE,
                        col.comb = 'red',
                        col.geom = 'blue',
                        col.coc  = 'orange',
                        ymax = NULL,
                        ...){
  dd <- COC.diff(F, ...)
  coc.near <- ifelse(is.null(cmax),
                     COC.geom(near, d, f, F) + COC.diff(F, ...),
                     cmax)
  if(dd<=coc){
    nr <- near.ok(d,f,F,coc)
    fr <- far.ok(d,f,F,coc)
    range <- c(nr,fr)
    if(!is.null(cmax)){
      near <- d - 2*(d-nr)
      far  <- d + 2*(fr-d)
    }

  } else {
    range <- NA
  }
  if(!hasArg(add)){
    comb <- curve(COC.comb(x,d,f,F),
                  near,
                  far,
                  col  = col.geom,
                  n    = 301,
                  ylim = c(0,
                           ifelse(is.null(ymax),
                                  coc.near,
                                  ymax)),
                  yaxs = 'i',
                  las  = 1,
                  type = 'n',
                  xlab = 'Distance [m]',
                  ylab = 'Circle of Confusion [mm]',
                  main = 'Focus Bluring',
                  ...)
    pu <- par('usr')
    rect(pu[1], pu[3], min(range), pu[4],
         col = grey(0.980), border = NA)
    rect(max(range), pu[3], pu[2], pu[4],
         col = grey(0.980), border = NA)
    abline(h = coc, col = col.coc, lty = 2)
  }
  if(geom){
    curve(COC.geom(x,d,f,F),
          near,
          far,
          n   = 301,
          col = 'blue',
          add = TRUE)
  }
  curve(COC.comb(x,d,f,F),
        near,
        far,
        n   = 301,
        col = col.comb,
        add = TRUE)
  abline(v   = range,
         col = 'darkgrey',
         lty = 3)
  rug(range)
  sel <- 1:2
  if(!geom) sel <- 1
  legend('topright',
         c("with diffraction","geometric only")[sel],
         lwd = 1,
         lty = 1, 
         col = c(col.comb,col.geom)[sel],
         bty = 'n',
         inset = 0.01)
  return(range)
}

# normalized aperture sequence (approx. f1 below)
f.seq <- 2^seq(0,5,by = 0.5)
# regular aperture sequences (1..32)
f0 <- 2^(0:5)                                              # integral powers = double steps
f1 <- sort(c(f0, c(1.4,2.8,5.6, 11, 22)))                  # full steps
f2 <- sort(c(f1, c(1.2,1.8,2.5,3.5,4.5,6.7,9.5,13,19,27))) # half steps
f3 <- sort(c(f1, c(1.1,1.2, 1.6,1.8, 2.2,2.5, 3.2,3.5,    # thirds
                 4.5,5, 6.3,7.1, 9,10, 13,14, 18,20, 25,29)))


#' Diffraction related to aperture and color
#'
#' @param max.F maximm aperture to consider
#'
#' @return
#' @export
#'
#' @examples
frac.lines <-  function(max.F = 32){
    # from: https://www.hug-technik.com/inhalt/ta/farben.html
    # Farbname	      Wellenlängenbereich (nm)
    # Purpurblau	    380–450
    # Blau	          450–482
    # Grünlich-Blau  	482–487
    # Cyan (Blau)	    487–492
    # Bläulich-Grün	  492–497
    # Grün	          497–530
    # Gelblich-Grün	  530–560
    # Gelb-Grün	      560–570
    # Grünlich-Gelb	  570–575
    # Gelb	          575–580
    # Gelblich-Orange	580–585
    # Orange        	585–595
    # Rötlich-Orange	595–620
    # Rot	            620–780
    lambdas <- c(750, 700, 660,630,600, 580,540, 500, 460, 420, 380)
    suppressWarnings(    # would complain about axes
      for (i in seq_along(lambdas)){
        curve(COC.diff(x, lambdas[i]),
              1,
              max.F,
              col  = w2rgb(lambdas[i]),
              log  = 'x',
              xlab = 'F',
              ylab = 'Diameter Airy Disk [mm]',
              main = "Diffraction vs. Aperture by Wavelength (Color)",
              axes = FALSE,
              add  = i>1)
      }
    )
    rug(f2, ticksize = -0.02, col = "grey")
    axis(1, at = f1, labels = f1, col = "grey", col.axis= "grey", cex.axis = 0.9)
    axis(1, at = f0, labels = f0)
    axis(2, las = 1)
    half.f <- setdiff(f2,f1)
    mtext(half.f, at = half.f, line = 0.25, cex = 0.65, col = 'lightgrey', side = 1)
    box()
    abline(v=f2, col = 'lightgrey', lty = 3)
    abline(v=f1, col = 'darkgrey', lty = 3)

    abline(h=c(4.3, 5.4, 18.6, 30)/1000, col = 'orange', lty = 3:2)
    mtext(c('Pixel size', 'CoC'), side = 4, at = c(4.85, 24.3)/1000, col = 'orange', cex = 0.7)
    legend('bottomright',
           c("Full frame", "APS-C"), cex = 0.7, text.col = 'grey',
           lty = 2:3, lwd = 1, col = "orange",
           bty = 'n', horiz = TRUE)
    legend('topleft',
           legend  = lambdas,
           cex     = 0.8,
           lty     = 1,
           lwd     = 1,
           col     = sapply(lambdas, w2rgb),
           box.col = 'darkgrey',
           inset   = 0.02,
           title   = expression(paste(lambda,' [nm]')))
}

#' Wavelength to RGB
#'
#' Convert wavelength to color, currently offering three conversion schemes.
#' The schemes differ in the end regions of the visible spectrum (UV and IR).
#' @param w wavelength [nm] as scalar
#' @param gamma gamma
#' @param components return as components
#' @param version which version of conversion to use (1..3)
#'
#' @return RGB color (unless \code{components} is TRUE, in which case a vector
#' \code{[r,g,b]} is returned)
#' @export
#'
#' @examples
w2rgb <- function(w,
                  gamma      = 0.8,
                  components = FALSE,
                  version    = 2){
    switch(version,
           {
               # adopted form
               # https://www.johndcook.com/w_to_RGB.html
               if (w >= 380 && w < 440)
               {
                   R = -(w - 440) / (440 - 380);
                   G = 0.0;
                   B = 1.0;
               }
               else if (w >= 440 && w < 490)
               {
                   R = 0.0;
                   G = (w - 440) / (490 - 440);
                   B = 1.0;
               }
               else if (w >= 490 && w < 510)
               {
                   R = 0.0;
                   G = 1.0;
                   B = -(w - 510) / (510 - 490);
               }
               else if (w >= 510 && w < 580)
               {
                   R = (w - 510) / (580 - 510);
                   G = 1.0;
                   B = 0.0;
               }
               else if (w >= 580 && w < 645)
               {
                   R = 1.0;
                   G = -(w - 645) / (645 - 580);
                   B = 0.0;
               }
               else if (w >= 645 && w < 781)
               {
                   R = 1.0;
                   G = 0.0;
                   B = 0.0;
               }
               else
               {
                   R   = 0.0;
                   G = 0.0;
                   B  = 0.0;
               }
           },
           {
               # adopted form
               # https://www.noah.org/wiki/w_to_RGB_in_Python,
               # in turn from
               # http://www.physics.sfasu.edu/astro/color/spectra.html
               if (w >= 380 && w <= 440){
                   attenuation = 0.3 + 0.7 * (w - 380) / (440 - 380)
                   R = ((-(w - 440) / (440 - 380)) * attenuation)^gamma
                   G = 0.0
                   B = (1.0 * attenuation)^gamma
               } else if (w >= 440 && w <= 490){
                   R = 0.0
                   G = ((w - 440) / (490 - 440))^gamma
                   B = 1.0
               } else if (w >= 490 && w <= 510){
                   R = 0.0
                   G = 1.0
                   B = (-(w - 510) / (510 - 490))^gamma
               } else if (w >= 510 && w <= 580){
                   R = ((w - 510) / (580 - 510))^gamma
                   G = 1.0
                   B = 0.0
               } else if (w >= 580 && w <= 645){
                   R = 1.0
                   G = (-(w - 645) / (645 - 580))^gamma
                   B = 0.0
               } else if (w >= 645 && w <= 750){
                   attenuation = 0.3 + 0.7 * (750 - w) / (750 - 645)
                   R = (1.0 * attenuation)^gamma
                   G = 0.0
                   B = 0.0
               } else {
                   R = 0.0
                   G = 0.0
                   B = 0.0
               }
           },
           {
               # adopted from
               # https://academo.org/demos/wavelength-to-colour-relationship/
               # base source no longer available
               if((w >= 380) && (w<440)){
                   R = -(w - 440) / (440 - 380);
                   G = 0.0;
                   B = 1.0;
               }else if((w >= 440) && (w<490)){
                   R = 0.0;
                   G = (w - 440) / (490 - 440);
                   B = 1.0;
               }else if((w >= 490) && (w<510)){
                   R = 0.0;
                   G = 1.0;
                   B = -(w - 510) / (510 - 490);
               }else if((w >= 510) && (w<580)){
                   R = (w - 510) / (580 - 510);
                   G = 1.0;
                   B = 0.0;
               }else if((w >= 580) && (w<645)){
                   R = 1.0;
                   G = -(w - 645) / (645 - 580);
                   B = 0.0;
               }else if((w >= 645) && (w<781)){
                   R = 1.0;
                   G = 0.0;
                   B = 0.0;
               }else{
                   R = 0.0;
                   G = 0.0;
                   B = 0.0;
               };
               # Let the intensity fall off near the vision limits
               if((w >= 380) && (w<420)){
                   factor = 0.3 + 0.7*(w - 380) / (420 - 380);
               }else if((w >= 420) && (w<701)){
                   factor = 1.0;
               }else if((w >= 701) && (w<781)){
                   factor = 0.3 + 0.7*(780 - w) / (780 - 700);
               }else{
                   factor = 0.0;
               }
               if (R != 0){
                   R = (R * factor)^gamma;
               }
               if (G != 0){
                   G = (G * factor)^gamma;
               }
               if (B != 0){
                   B = (B * factor)^gamma;
               }

           }
    )
    return(rgb(R,G,B))
}

#' plot spectrum
#'
#' @param yr y-range
#' @param new start new plot
#' @param ... pass to \code{\link{w2rgb}
#'
#' @return
#' @export
#'
#' @examples
plot.spectrum <- function(yr = 0:1,
                          new = TRUE,
                          ...){
    w.rng <- 360:800
    if(new){
        plot(w.rng,seq(0,1, length.out = length(w.rng)),
             type = 'n',
             axes = FALSE,
             main = 'Spectral colors',
             xlab = 'w [nm]',
             ylab = '')
        axis(1)
        box()
    }
    for (i in w.rng){
        rect(i-.5, yr[1], i+.5, yr[2], col = w2rgb(i, ...), border = NA)
    }
}

#' plot spectra
#'
#' compare wavelength to color conversion schemes as defined in \code{\link{w2rgb}}
#'
#' @param version \code{version} parameter in  \code{\link{w2rgb}
#'
#' @return none
#' @export
#'
#' @examples
#' plot.spectra()
plot.spectra <- function(version = 1:3){
    n <- length(version)
    for (i in seq_along(version)){
        plot.spectrum(v   = i,
                      yr  = 0:1/n + (i-1)/n,
                      new = i==version[1])
    }
    axis(2, at = 1:n/n - 1/(2*n), version, las = 1)
    title(ylab = "w2rgb version")
}

bessel <- function(r, center = 0, lambda = 550, F = 11){
  k <- 2*pi/lambda
  (2*besselJ(r, 1)/r)^2
  
}

#' rotate vector
#'
#' positive \code[i] rotates left (element i+1 becomes first)
#' For \code{i}=0, the original vector is returned
#' @param v vector
#' @param i rotate by this many elements
#'
#' @return rotated vector
#' @export
#'
#' @examples
#' rot(1:5, 2)   #  3 4 5 1 2
#'rot(1:5, -2)   #  4 5 1 2 3
rot <- function(v, i){
  if(i==0) return (v)
  ## i>0: rotate left
  n <- length(v)
  if (i>0){
    c(v[(1+i):n], v[1:i])
  } else {
    #browser()
    c(v[(n+1+i):n], v[1:(n+i)])
  }
}

#' shift vector with zero-fill
#'
#' positive \code[i] shifts left (element i+1 becomes first)
#' For \code{i}=0, the original vector is returned
#' @param v vector
#' @param i shift by this many elements
#'
#' @return shiftd vector
#' @export
#'
#' @examples
#' rot(1:5, 2)   #  3 4 5 1 2
#'rot(1:5, -2)   #  4 5 1 2 3
shift <- function(v, i){
  if(i==0) return (v)
  ## i>0: shift left
  n <- length(v)
  if (i>0){
    c(v[(1+i):n], rep(0, i))
  } else {
    #browser()
    c(rep(0,-i), v[1:(n+i)])
  }
  
}


#' calculate lineshape for extended image 
#'
#' approximate by summing point sources 0.01µm apart in [-w/2, w/2]
#' corresponding to a rectangular source profile
#' 
#' The \code{window} parameter is interpreted twice, once as plot window width 
#' ([-window, window]) and to calculate the lineshape 
#' in the interval [-2*window, 2*window].
#' 
#' The source profile is assumed to be 1 in {[-rect/2, rect/2]}, 0 otherwise.
#' 
#' The lineshape is calculated by adding pointwise lineshapes going 
#' from \code{+rect/2} to \code{-rect/2} in steps of \code{-step} and 
#' normalizing the result to have a maximum of 1.
#' 
#' The special case \code{rect==0} returns the pointwise lineshape.
#' 
#' @param F aperture
#' @param lambda wavelength         [nm]
#' @param rect width of rectangle   [µm]
#' @param window extend of image    [µm]
#' @param step discretization steps [µm]
#' @param verbose when set (1), plot lineshape (extended and pointwise),
#' when >1, also show approach
#' @param ... unused
#'
#' @return
#' @export
#'
#' @examples
#' ls <- lineshape(F=8, lambda=633, rect = 5.4, window = 5)
lineshape <- function(F       = 11,
                      lambda  = 550, 
                      rect    = 2,
                      window  = 10,
                      step    = 0.01,
                      verbose = getOption('verbose'),
                      ...){
  x <- seq(-2*window, 2*window, by = step)
  i0 <- bessel2(x, F = F, lambda = lambda, center = rect/2)
  
  if(verbose){
    plot(c(-window, window), 0:1, type = 'n')
    rect(-rect/2, 0, rect/2, 1, col = grey(0.9), border = NA)
  }
  if(verbose>1)
    lines(x, i0, col ='red')
  i <- i0
  n <- rect/step
  if(n>1){
    for (shift in 1:n){
      i <- i + shift(i0, shift)
      if(verbose > 1)
        lines(x, i/(shift+1), col = '#20202010')
    }
  }
  i <- i/max(i)
  if(verbose){
    grid()
    lines(x, i00 <- bessel2(x, F=F, lambda = lambda), col = 'cyan')
    lines(x, i, col = 'red', lwd = 2)
    rug(c(-1,1)*(m1 <- first.min(x, i)), col = 'red')
    rug(c(-1,1)*(m0 <- first.min(x, shift(i0, rect/2/step))), col = 'cyan')
  }  
  return(invisible(data.frame(x = x, y = i)))
}

#' First minimum of lineshape
#'
#' @param lineshape either result of \code{\link{lineshape}} or equivqlent x-vector
#' @param y y-vector when \code{lineshape} is given as vector
#' @param ... unused
#'
#' @return (positive) coordinate of first minimum
#' @export
#'
#' @examples
first.min <- function(lineshape,
                      y = NULL,
                      verbose = getOption('verbose'),
                      ...){
  xy <- xy.coords(lineshape, y)
  use <- xy$x>=0
  diffs <- diff(xy$y[use])
  min <- (xy$x[use])[i <- which.max(diffs>0)]
  if(verbose)
    cat(sprintf("%d: %f\n", i, min))
  return(min)
}

#' Title
#'
#' @param x x-coord  [µm]
#' @param F
#' @param lambda     [nm]
#' @param center     [µm]
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
bessel2 <- function(x,
                    F = 11,
                    lambda = 550, 
                    center = 0,
                    ...){
  ## removed factor 2 to make it consistent with values from COC.diff
  r <- pi/(lambda*1e-3 * F)*(x-center) 
  #cat(r,'\n')
  ifelse(r==0, 1, (2*besselJ(abs(r), 1)/r)^2)
  
}

#' Title
#'
#' @param F aperture
#' @param lambda wavelength
#' @param pixel pixel size
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot.airy <- function(F=11, lambda = 550, pixel = 5.4, ...){
  intensity <- curve(bessel, 0, 8, 
                     axes = FALSE,
                     xlab = "r [µm]",
                     ylab = 'relative intensity')
  xv <- lambda * 1e-9 * F / 2 /pi * 0:8
  mu <- 2*pi*1e-6/F/(lambda* 1e-9)
  n.tick <- (8%/%mu)
  ticks <- 0:n.tick
  axis(1, at = ticks*mu, lab = ticks)
  axis(2, las = 1)
  abline(v = ticks*mu, col = "lightgray", lty = "dotted")
  grid()
  box()
  return(invisible(intensity))
}
  

#' Show Overlap of two Airy disks
#'
#' Assuming two objects whose images are separated by one pixel on the sensor,
#' show how the the corresponding Airy disks overlap
#' In order to estimate the achievable resolution it is assumed that in the image
#' two bright pixels are separated by one dark pixel. This corresponds to something 
#' like a Nyquist distance on the sensor.  
#' For \code{src==0} two point sources are assumed for which the
#' ideal image points lie in the center of the two "bright' pixels, for \code{src==1}, 
#' a rectangular input profile is assumed for which the ideal image would align perfectly
#' to the pixels.
#' 
#' Links:
#' - www.cambridgeincolour.com/tutorials/diffraction-photography.htm
#' - spie.org/publications/fg01_p88_airy_disk
#' @param lambda wavelength
#' @param pixel pixel dimension on camera (5D 5.4, 6D: 6.5, 7D: 4.3)
#' @param type point[0]/rectangular[1] source
#' @param col color for Airy functions
#' @param col.rel color for relative intensities
#' @param d separation of source centers
#' @param xr 
#' @param n 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
airy2 <- function(F       = 11,
                  lambda  = 550,
                  pixel   = 5.4, 
                  type    = 0, #c('point', 'rectangular'),
                  col     = w2rgb(lambda),
                  col.rel = adjustcolor(col, 1, 0.75,0.75,0.75),
                  d       = 2*pixel, 
                  xr      = 2*pixel,
                  res     = 0.01, 
                  add     = FALSE,
                  ...){
  ##xs <- seq(0, xr, length.out = n)
  ## i1 <- bessel2(xs, F, lambda, center = 0)
  ## i2 <- bessel2(xs, F, lambda, center = d)
  i1 <- lineshape(F       = F, 
                  lambda  = lambda,
                  rect    = pixel*type, 
                  window  = xr, 
                  src     = type)
  i2 <- i1
  i2$y <- shift(i2$y, -2*pixel/res)
  isum <-  i1$y+i2$y
  if (!add){
    plot(i1$x, isum, 
         col = col, 
         type = 'l',
         xlim = c(0, xr),
         ylim = c(0, max(isum)),
         xlab = "r [µm]",
         ylab = 'relative intensity')
  } else{
    lines(i1$x, isum, col = col)
  }
  lines(i1, 
        col = adjustcolor(col, 0.5), 
        lty = 2, 
        lwd = 2)
  lines(i2, 
        col = adjustcolor(col, 0.5), 
        lty = 3, 
        lwd = 2)
 
 
  ph <- pixel/2
  if(d==2*pixel && xr>pixel){
    parts <- i1$x%/%ph
    mxp <- max(parts)
    abline(v=c(1, 3)*ph, lty = 3, col = 'lightgrey')
    rel.intens <- tapply(isum, parts, sum)[c('0', '1', '2', '3')]
    rel.intens <- (rel.intens + rev(rel.intens))/2
    ri <- rel.intens/rel.intens[1] 
    ri2 <- ri * tapply(isum, parts, mean)['0']
    
    segments(0:3*ph, ri2[1:4], 1:4*ph, ri2[1:4], col = col.rel)
    axis(4,at = ri2[2], labels = sprintf("%.1f%%", 100*ri[2]),
         cex.axis = 0.5, las = 1, mgp = c(3,.5,0), tick = FALSE)
    rug( ri2[1:2], -0.01, 4)
  }
  #browser()
  return(invisible(cbind(i1, i2$y, isum)))
  
}

#' Spectral Airy2 plot
#'
#' @param F aperture
#' @param lambda wavelengths (vector)
#' @param pixel pixel dimension
#' @param src o for point, 1 for rectangular source profile
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
airy2.spectrum <- function(F = 11, 
                          lambda = c(750, 700, 660,630,600, 580,540, 500, 460, 420, 380),
                          pixel = 5.4,
                         ...){
  for(li in seq_along(lambda)){
    airy2(F=F, lambda = lambda[li], add = li > 1, pixel = pixel, ...)
  }
}
