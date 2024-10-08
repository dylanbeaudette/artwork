library(aqp)
library(lattice)
library(tactile)

data(munsell.spectra)


x <- munsell.spectra

# each Munsell chip has a 36-element spectra
# ranging from 380-730 nm
# table(x$munsell)

# spectra IDs
x$ID <- factor(x$munsell)

tps <- tactile.theme(
  background = list(col = 'black'), 
  superpose.line = list(col = scales::alpha('white', 0.05), lwd = 0.5)
)

xyplot(
  reflectance ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  type = c('l'),
  ylab = '',
  xlab = '',
  scales = list(draw = FALSE),
  xlim = c(370, 740)
)



x <- munsell.spectra[munsell.spectra$value  == 6, ]

x <- munsell.spectra[munsell.spectra$chroma  == 6, ]

# each Munsell chip has a 36-element spectra
# ranging from 380-730 nm
# table(x$munsell)

# spectra IDs
x$ID <- factor(x$munsell)
# create a color / chip
cols <- scales::alpha(parseMunsell(as.character(levels(x$ID))), 0.2)

# plot style
tps <- tactile.theme(superpose.line = list(col = cols, lwd = 2))

# final figure
xyplot(
  reflectance ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  main = 'Value 6',
  type = c('l', 'g'),
  ylab = 'Reflectance',
  xlab = 'Wavelength (nm)',
  scales = list(tick.number = 12),
  xlim = c(370, 740)
)



x <- munsell.spectra[munsell.spectra$value  %in% c(6, 8), ]

# each Munsell chip has a 36-element spectra
# ranging from 380-730 nm
# table(x$munsell)

# spectra IDs
x$ID <- factor(x$munsell)
# create a color / chip
cols <- scales::alpha(parseMunsell(as.character(levels(x$ID))), 0.2)

# plot style
tps <- tactile.theme(
  background = list(col = 'black'), 
  superpose.line = list(col = cols, lwd = 3)
)



xyplot(
  reflectance ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  # main = '',
  type = c('l'),
  ylab = 'Reflectance',
  xlab = 'Wavelength (nm)',
  scales = list(draw = FALSE),
  xlim = c(370, 740)
)




x <- munsell.spectra[munsell.spectra$chroma  > 6, ]

# each Munsell chip has a 36-element spectra
# ranging from 380-730 nm
# table(x$munsell)

# spectra IDs
x$ID <- factor(x$munsell)
# create a color / chip
cols <- scales::alpha(parseMunsell(as.character(levels(x$ID))), 0.2)

# plot style
tps <- tactile.theme(
  background = list(col = 'black'), 
  superpose.line = list(col = cols, lwd = 3)
)

xyplot(
  reflectance ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  # main = '',
  type = c('l'),
  ylab = 'Reflectance',
  xlab = 'Wavelength (nm)',
  scales = list(draw = FALSE),
  xlim = c(370, 740)
)





x <- munsell.spectra[munsell.spectra$chroma < 2, ]

# each Munsell chip has a 36-element spectra
# ranging from 380-730 nm
# table(x$munsell)

# spectra IDs
x$ID <- factor(x$munsell)
# create a color / chip
cols <- scales::alpha(parseMunsell(as.character(levels(x$ID))), 0.33)

# plot style
tps <- tactile.theme(
  background = list(col = 'black'), 
  superpose.line = list(col = cols, lwd = 3)
)


xyplot(
  reflectance ~ wavelength, groups = ID, data = x, 
  par.settings = tps,
  # main = '',
  type = c('l'),
  ylab = 'Reflectance',
  xlab = 'Wavelength (nm)',
  scales = list(draw = FALSE),
  xlim = c(370, 740)
)




