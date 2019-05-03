
Fires <- function(place, Title){      # Actual fires == Example: Fires(CLAWR$PROP_BURN, "CLAWR Fires")
  plot(place$YEAR, place$PROP_BURN, xlab = "", ylab= "",
       ylim = c(0,max(place$PROP_BURN)), type = "h", cex.axis = 1.2)
  title(main = Title, xlab = "Year", ylab = "Proportion Area Burned",
        font.main = 2, font.lab = 2,
        col.main = "Black", col.lab = "Black",
        cex.lab = 1.3, cex.main = 1.4)
}