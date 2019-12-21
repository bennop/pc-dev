# Confucion circles

#' Geometric circle of confusion
#'
#' Calculate the diameter of the circle of confusion (COC)
#' as a function of distance given the parameters focus distance,
#' focal length, and aperture derived from geometric considerations.
#'
#' Diffraction is ignored, see \code{\link{COC.comb}} for a combined treatment.
#'
#' @param x distance parameter
#' @param d focus distance
#' @param f focal length of lens
#' @param F aperture
#' @param ... ignored
#'
#' @return Diameter of geometric COC
#' @export
#'
#' @examples
COC.geom <- function(x,d,f,F,...){
  return(f^2/F*abs(d-x)/(x*(d-f)))
}

#' Confusion caused by diffraction
#'
#' Returns the diameter of the Airy disc (first minimum) for the given aperture
#'
#' @param lambda wavelength
#' @param ... ignored
#'
#' @return
#' @export
#'
#' @examples
COC.diff <- function(F, lambda = 500, ...){
  # lambda in nm (= 1e-6 mm)
  return(2.44*(lambda*1e-6)*F)
}

# curve(COC.comb(x,1000,50,16),700,1400,  col='R', add = TRUE)

#' Combined Circle of Confusion
#'
#' Consider the combined effects of geometry (from \code{\link{COC.geom}})
#' and diffraction (from \code{\link{COC.diff}}) on the blurring of an
#' acquired image
#'
#' @param x distance
#' @param d focus distance
#' @param f focal length
#' @param F aperture
#' @param ... passed to \code{\link{COC.diff}}
#'
#' @return diameter of the effective COC
#' @export
#'
#' @examples
COC.comb <- function(x, d, f, F, ...){
  #return(1/sqrt(COC.geom(x, d, f, F)^(-2) + COC.diff(F)^(-2)))
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
near.ok <- function(d, f, F, coc = 0.03, ...){
    return(ifelse(coc < COC.diff(F, ...),
                  uniroot(function(x)COC.comb(x, d, f, F, ...) - coc, interval = c(f, d))$root,
                  NA))
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
#' @param d focus distance
#' @param f focal length
#' @param F aperture
#' @param coc target COC
#' @param ... passed to \code{\link{COC.diff}}
#'
#' @return maximum distance in focus [mm]
#' @export
#'
#' @examples
far.ok <- function(d, f, F, coc = 0.03, ...){
    return(ifelse(coc < COC.diff(F, ...),
                  uniroot(function(x)COC.comb(x, d, f, F, ...) - coc, interval = c(d, 100*d))$root,
                  NA))
}

#' Depth of Field (DoF)
#'
#' Calculatwe the effective depth of field given the parameters
#'  focus distance, focal length, aperture, and target COC.
#'
#' @param d focus distance
#' @param f focal length
#' @param F aperture
#' @param coc target COC
#' @param ... passed to \code{\link{COC.diff}}
#'
#' @return DOF im mm
#' @export
#'
#' @examples
dof <- function(d, f, F, coc = 0.03){
  far.ok(d, f, F, coc) - near.ok(d, f, F, coc)
}

DOF.plot <- function(d, f, F,
                     near = d/2,
                     far  = 2*d,
                     coc  = 0.03,
                     cmax = NULL,
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
  curve(coc(x,d,f,F),
        near,
        far,
        col  = 'blue',
        n    = 301,
        ylim = c(0, coc.near),
        yaxs = 'i',
        las  = 1)
  abline(h=coc, col = 'orange', lty = 2)
  curve(COC.comb(x,d,f,F),
        near,
        far,
        n   = 301,
        col = 'red',
        add = TRUE)
  abline(v   = range,
         col = 'darkgrey',
         lty = 3)
  rug(range)
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
#' @param max.F
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
    suppressWarnings(for (i in seq_along(lambdas)){
        curve(COC.diff(x, lambdas[i]),
              1,
              max.F,
              col  = w2rgb(lambdas[i]),
              log  = 'x',
              xlab = 'F',
              ylab = 'Diameter Airy Disk [mm]',
              main = "Diffraction",
              axes = FALSE,
              add  = i>1)
    })
    rug(f2, ticksize = -0.02, col = "grey")
    axis(1, at = f1, labels = f1, col = "grey", col.axis= "grey")
    axis(1, at = f0, labels = f0)
    axis(2, las = 1)
    box()
    abline(v=f2, col = 'lightgrey', lty = 3)
    abline(v=f1, col = 'darkgrey', lty = 2)

    abline(h=c(4.3, 5.4, 18.6, 30)/1000, col = 'orange', lty = 3:2)
}

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
