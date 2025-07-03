### EDF SD Map Figures Script



# when I have to make plots
map_palatte = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
breaks = c(0,10^seq(0,log10(1000),length=9))
#terra::plot(Reg,"cropMean",legend="topright", breaks = breaks, col = palatte)
terra::plot(Reg, "crop_Tot_SD", legend = "topright", breaks = breaks, col = palatte)
terra::plot(Reg, "crop_ensVar_SD", legend = "topright", breaks = breaks, col = palatte)
