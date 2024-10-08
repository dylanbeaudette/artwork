
# Thanks to Nate Roe for suggesting the name, and entertaining my odd outbursts of "artistic" expression.

# Maybe this is more useful than originally joked about.

# https://pubmed.ncbi.nlm.nih.gov/6965723/

# ask about embossing printers
# https://www.usda.gov/our-agency/staff-offices/office-communications-oc/printing-services

##
# https://louis.aph.org/
# https://www.afb.org/about-afb/contact-afb

## note that numbers require "#" prefix in Braille



library(aqp)
library(sharpshootR)
library(showtext)
library(lattice)
library(svglite)


# remotes::install_github("JosephCrispell/basicPlotteR")
library(basicPlotteR)

font_add('BRAILLE1', regular = 'BRAILLE1.ttf')

showtext_auto()


par(mar = c(1, 1, 1, 1), family = 'sans', bg = 'black', fg = 'white')

par(mar = c(1, 1, 1, 1), family = 'BRAILLE1', bg = 'black', fg = 'white')

# par(mfcol = c(1, 2),mar = c(1, 1, 1, 1), family = 'sans', bg = 'black', fg = 'white')

huePositionCircle(chip.cex = 7.5, label.cex = 0.8)

huePositionPlot()




svglite(filename = 'e:/working_copies/ncss-tech.github.io/AQP/SVG-examples/munsell-hues-braille.svg', width = 10, height = 10, bg = 'transparent')

font_add('BRAILLE1', regular = 'BRAILLE1.ttf')
font_add('morse', regular = 'MorseTech-n4E4.ttf')

showtext_auto()

par(mar = c(1, 1, 1, 1), family = 'BRAILLE1')

huePositionCircle(chip.cex = 8, label.cex = 0.75)

dev.off()


.f <- list(fontfamily = 'BRAILLE1')
tps <- list(
  par.xlab.text = .f,
  par.ylab.text = .f,
  axis.text = .f,
  par.sub.text = .f,
  add.text = .f,
  par.main.text = .f
)

trellis.par.set(tps)
contrastChart('10YR 3/3', hues = c('10YR', '5Y'), ccAbbreviate = 0, de00.cex = 0.8)

contrastChart('10YR 3/3', hues = c('10YR', '5Y'), ccAbbreviate = 0, de00.cex = 0)


colorChart(c('N2/', '10YR 3/3', '5YR 4/6', '10PB 6/6', '10YR 3/4'))


par(family = 'BRAILLE1')
huePositionPlot(value = 4, chroma = 6, chip.cex = 6, label.cex = 0.65, contour.dE00 = TRUE, origin = '10YR 3/3')


## Munsell -> CIELAB charts

# multiple pages of hue:
hues <- c('2.5YR', '5YR', '7.5YR', '10YR', '2.5Y')
d <- expand.grid(hue=hues, value=2:8, chroma=c(1, 2, 3, 4, 6, 8), stringsAsFactors=FALSE)
d$hue <- factor(d$hue, levels=hues, ordered = TRUE)

# convert Munsell -> sRGB
d$color <- with(d, munsell2rgb(hue, value, chroma))

# extract CIELAB coordinates
d.lab <- with(d, munsell2rgb(hue, value, chroma, returnLAB=TRUE))

# this is lame, there has to be a better way
d$L <- d.lab$L
d$A <- d.lab$A
d$B <- d.lab$B

# adjust color label text according to background color
d$lab.color <- invertLabelColor(d$color)
# simplified CLIE LAB labels for printing on "chips"
d$lab.text <- with(d, paste(round(L), round(A), round(B), sep='\n'))



data("munsell")
data("soil_minerals")

x <- munsell[which(munsell$value == 6 & munsell$chroma == 6), ]

x.1 <- subset(munsell, subset=hue == '10YR' & value == 2 & chroma  == 2)
x.2 <- subset(munsell, subset=hue == '7.5YR' & value == 3 & chroma  == 4)

par(mar = c(0, 0, 0, 0), family = 'BRAILLE1')

plot(B ~ A, data=x, type='n', las=1, asp = 1)
grid()
abline(h=0, v=0, lty=3)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=4)
addTextLabels(xCoords = x$A, yCoords = x$B, labels = sprintf("%s %s/%s", x$hue, x$value, x$chroma), cex.label = 0.7, col.background = rgb(0,0,0, 0.1), col.label="black")


# par(bg=grey(0.95), mar=c(4.5,4.5,3,1))
par(bg='white', mar=c(4.5,4.5,3,1))
plot(B ~ A, data=x, type='n', las=1, main='Munsell Colors in CIELAB\nvalue = 4 | chroma = 6', asp = 1)
grid(col='black')
abline(h=0, v=0, lty=1)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=4)
addTextLabels(x$A, x$B, x$hue, cex.label = 0.7, col.background=rgb(0,0,0, 0.1), col.label="black")

points(B ~ A, data=x.1, col=rgb(x.1$r, x.1$g, x.1$b, maxColorValue = 1), pch=15, cex=4)
points(B ~ A, data=x.2, col=rgb(x.2$r, x.2$g, x.2$b, maxColorValue = 1), pch=15, cex=4)
addTextLabels(x.1$A, x.1$B, sprintf("%s %s/%s", x.1$hue, x.1$value, x.1$chroma), cex.label = 0.7, col.background=rgb(0,0,0, 0.1), col.label="black")
addTextLabels(x.2$A, x.2$B, sprintf("%s %s/%s", x.2$hue, x.2$value, x.2$chroma), cex.label = 0.7, col.background=rgb(0,0,0, 0.1), col.label="black")


par(bg='white', mar=c(4.5,4.5,3,1))
plot(B ~ A, data=x, type='n', las=1, main='Munsell Colors in CIELAB\nvalue = 4 | chroma = 6')
grid(col='black')
abline(h=0, v=0, lty=1)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=2)
addTextLabels(x$A, x$B, x$hue, cex.label = 0.7, col.background=rgb(0,0,0, 0.1), col.label="black")

m.rgb <- t(col2rgb(parseMunsell(soil_minerals$color))) / 255
m.lab <- convertColor(m.rgb, from='sRGB', to='Lab', from.ref.white = 'D65', clip = FALSE)
m.lab <- as.data.frame(m.lab)
names(m.lab) <- c('L', 'A', 'B')
m <- data.frame(m.lab, mineral=soil_minerals$mineral, munsell=soil_minerals$color, col=parseMunsell(soil_minerals$color), stringsAsFactors = FALSE)

points(B ~ A, data=m, col=m$col, pch=15, cex=3)
addTextLabels(m$A, m$B, m$mineral, cex.label = 0.6, col.background=rgb(0,0,0, 0.1), col.label="black")


par(bg='white', mar=c(4.5,4.5,3,1))
plot(B ~ A, data=m, type='n', las=1, main='Common Soil Pigments', xlab='CIELAB A-Coordinate', ylab='CIELAB B-Coordinate', asp = 1)
grid(col='black')
points(B ~ A, data=m, bg=m$col, pch=22, cex=6)
addTextLabels(m$A, m$B, m$mineral, cex.label = 0.7, col.background=rgb(0,0,0, 0.1), col.label="black")

par(bg='white', mar=c(4.5,4.5,3,1))
plot(L ~ A, data=m, type='n', las=1, main='Common Soil Pigments', xlab='CIELAB A-Coordinate', ylab='CIELAB L-Coordinate', asp = 1)
grid(col='black')
points(L ~ A, data=m, bg=m$col, pch=22, cex=6)
addTextLabels(m$A, m$L, m$mineral, cex.label = 0.7, col.background=rgb(0,0,0, 0.1), col.label="black")



x <- subset(munsell, subset=hue %in% c('7.5YR', '10YR') & value %in% 2:8 & chroma %in% 2:8)

plot(B ~ A, data=x, type='n', las=1, asp = 1)
grid()
abline(h=0, v=0, lty=3)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=4)
addTextLabels(xCoords = x$A, yCoords = x$B, labels = sprintf("%s %s/%s", x$hue, x$value, x$chroma), cex.label = 0.7, col.background = rgb(0,0,0, 0.1), col.label="black")

par(bg='white', mar=c(4.5,4.5,3,1))
plot(B ~ A, data=x, type='n', las=1)
grid(col='black')
abline(h=0, v=0, lty=1)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=2)
addTextLabels(x$A, x$B, x$hue, cex.label = 0.7, col.background=rgb(0,0,0, 0.1), col.label="black")








