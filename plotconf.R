plotconf <- list(mar = c(3, 3.5, 2, 1), tcl = 0.3, mgp = c(1.5, 0.2, 0), las = 1, ylbias = 0.38)
ypos <- function(pos = 2) {
  vals <- as.character(pretty(axTicks(pos)))
  nc <- max(strwidth(vals, units = "in"))
  return(nc)
}

# Add ylabel
ylabel <- function(lab = "y", cst = 0.65,
                   las = 0, cex.lab = par()$cex.lab,
                   cex = par()$cex, ...) {
  conv <- na.omit((par()$mar/par()$mai))[1]
  mtext(side = 2, line = ypos(2) * conv + cst,
        text = eval(parse(text = deparse(substitute(lab))), parent.frame()),
        las = las, xpd = NA, cex.lab = cex.lab, cex = cex, ...)
}
mbox <- function(atx = NULL, aty = NULL) {
  if(!is.null(atx)) {
    if (class(atx) == "Date") {
      axis.Date(3, x = atx, lab = NA)
    } else {
      axis(3, at = atx, lab = NA)
    }
  } else {
    axis(3, lab = NA)
  }

  if(!is.null(aty)) {
    axis(4, at = aty, lab = NA)
  } else {
    axis(4, lab = NA)
  }
  box()
}
cb <- c(black = "#000000", grey = "#999999", orange = "#E69F00",
        turquoise = "#56B4E9", green = "#009E73", yellow = "#F0E442",
        blue = "#0072B2", red = "#D55E00", pink = "#CC79A7")
