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
              main = "Diffraction vs. Wavelength (color)",
              axes = FALSE,
              add  = i>1)
    })
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
           legend = lambdas,
           cex = 0.8,
           lty = 1,
           lwd = 1,
           col = sapply(lambdas, w2rgb),
           box.col = 'darkgrey',
           inset = 0.02,
           title = expression(paste(lambda,' [nm]')))
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
