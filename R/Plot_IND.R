
pHoof <- function(place, Title){      # Hoof print = Example: pHoof(CLAWR$HOOF, "CLAWR Industrial Footprint")
  plot(place$YEAR, place$HOOF, xlab = "", ylab= "",
       ylim = c(0,1),
       type = "h",
       cex.axis = 1.2)
  title(main = Title,
        xlab = "Year", ylab = "%IND",
        font.main = 2, font.lab = 2,
        col.main = "black", col.lab = "black",
        cex.lab = 1.3, cex.main = 1.4)
}
