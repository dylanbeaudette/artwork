library(aqp)
library(soilDB)
library(sharpshootR)
library(farver)
library(cluster)
library(WeightedTreemaps)

## TODO:
# * ensure original colors are used
# * better control over clustering / distance metric
# * 2D distance field for init.
# * allow vector of colors / or auto-inversion of colors based on luminance


p <- list(
  'A' = list(hvc = data.frame(
    hue = c('10YR', '10YR', '7.5YR', '7.5YR', '2.5YR', '5PB', '10YR', '10YR', '10YR'),
    value = c(3, 4, 3, 4, 3, 2, 6, 2, 2),
    chroma = c(4, 5, 6, 6, 6, 4, 2, 2, 3)
  )
  )
)


cols <- simulateColor(method = 'mvnorm', n = 100, parameters = p)

cols <- stack(cols)
colorChart(cols$values, size = TRUE)

x <- table(cols$values)
x <- as.data.frame(x)
names(x) <- c('munsell', 'Freq')
x$munsell <- as.character(x$munsell)
x$col <- parseMunsell(x$munsell)

.lab <- parseMunsell(x$munsell, returnLAB = TRUE)
d <- compare_colour(
  from = .lab, 
  to = .lab, 
  from_space = 'lab', 
  to_space = 'lab', 
  white_from = 'd65', 
  white_to = 'd65', 
  method = 'cie2000'
)

dimnames(d)[[1]] <- x$munsell
dimnames(d)[[2]] <- x$munsell

d <- as.dist(d)
h <- diana(d)

x$munsell <- factor(x$munsell, levels = x$munsell[h$order])
x <- x[order(x$munsell), ]

tm <- voronoiTreemap(
  data = x,
  levels = c('munsell'), 
  sort = FALSE,
  positioning = 'regular', 
  cell_size = 'Freq', 
  fun = sum
)

drawTreemap(
  tm, 
  color_level = 1,
  color_type = 'categorical', 
  color_palette = x$col, border_color = grey(0.8),
  border_size = 0.5,
  label_size = 3, 
  label_autoscale = TRUE
)


##
p <- list(
  'A' = list(m = '10YR 4/2', thresh = 5, hues = c('10YR')),
  'E' = list(m = '10YR 6/4', thresh = 5, hues = c('10YR')),
  'Bt1' = list(m = '10YR 6/4', thresh = 8, hues = c('10YR')),
  'Bt2' = list(m = '10YR 5/6', thresh = 8, hues = c('10YR', '7.5YR')),
  '2Bt3' = list(m = '10YR 5/6', thresh = 8, hues = c('10YR', '7.5YR')),
  '2Bt4' = list(m = '7.5YR 5/6', thresh = 10, hues = c('7.5YR', '5YR')),
  '3Bt5' = list(m = '2.5YR 4/6', thresh = 10, hues = c('2.5YR', '5YR'))
)

# simulate
(cols <- simulateColor(method = 'dE00', n = 20, parameters = p))

# preview
previewColors(parseMunsell(unlist(cols)), method = 'MDS')

cols <- stack(cols)

names(cols) <- c('munsell', 'hz')

x <- as.data.frame(xtabs(~ hz + munsell, data = cols))

x <- subset(x, subset = Freq > 0)
x$munsell <- as.character(x$munsell)

x$col <- parseMunsell(x$munsell)

x <- x[order(x$hz, x$munsell), ]

colorChart(cols$munsell, g = cols$hz)


tm <- voronoiTreemap(
  data = x,
  levels = c('hz', 'munsell'), 
  sort = FALSE,
  positioning = 'regular', 
  cell_size = 'Freq', 
  fun = sum
)

drawTreemap(
  tm, 
  color_level = 2,
  color_type = 'categorical', 
  color_palette = x$col, border_color = grey(0.8),
  border_size = 0.5, label_level = 2,
  label_size = 3, 
  label_autoscale = TRUE
)


##


p <- list(
  'A' = list(m = '7.5YR 4/6', thresh = 15, hues = c('10YR', '7.5YR', '2.5YR', '2.5Y'))
)

cols <- simulateColor(method = 'dE00', n = 100, parameters = p)

# flatten
cols <- unlist(cols)

x <- as.data.frame(table(cols))
names(x) <- c('munsell', 'freq')

x <- rbind(x, data.frame(munsell = '5PB 4/6', freq = 10))

x$col <- parseMunsell(x$munsell)
x$colID <- 1:nrow(x)

.lab <- parseMunsell(x$munsell, returnLAB = TRUE)
d <- compare_colour(
  from = .lab, 
  to = .lab, 
  from_space = 'lab', 
  to_space = 'lab', 
  white_from = 'd65', 
  white_to = 'd65', 
  method = 'cie2000'
)

dimnames(d)[[1]] <- x$munsell
dimnames(d)[[2]] <- x$munsell

d <- as.dist(d)
h <- diana(d)

x$munsell <- factor(x$munsell, levels = x$munsell[h$order])
x <- x[order(x$munsell), ]

nrow(x)

previewColors(x$col, method = 'MDS')


tm <- voronoiTreemap(
  data = x,
  levels = c('munsell'), 
  sort = FALSE,
  positioning = 'regular', 
  custom_color = 'colID',
  cell_size = 'freq', 
  fun = sum
)

drawTreemap(
  tm, 
  color_level = 1,
  color_type = 'categorical', 
  color_palette = x$col, 
  border_size = 0.5,
  label_size = 3, 
  label_autoscale = TRUE
  # label_color = invertLabelColor(x$col)
)



x <- data.frame(
  name = c('A', 'A', 'B', 'C', 'D', 'D', 'E', 'F', 'F'),
  munsell = c('10YR 3/1', '5Y 6/1', '5R 3/6', '10YR 3/1', '5B 4/4', '2.5YR 3/4', '5G 3/4', '10YR 3/2', '10YR 6/8')
)



# x <- OSDexamples$SPC
# x <- as.data.frame(x)
# x <- subset(x, subset = !is.na(hue) & !is.na(value) & !is.na(chroma))
# x$munsell <- sprintf('%s %s/%s', x$hue, x$value, x$chroma)
# x$thick <- x$bottom - x$top

# x$col <- parseMunsell(x$munsell)

m <- data.frame(
  munsell = unique(x$munsell)
)

.lab <- parseMunsell(m$munsell, returnLAB = TRUE)

d <- compare_colour(.lab, .lab, from_space = 'lab', to_space = 'lab', white_from = 'd65', white_to = 'd65', method = 'cie2000')

dimnames(d)[[1]] <- m$munsell
dimnames(d)[[2]] <- m$munsell

d <- as.dist(d)

h <- diana(d)

h$order

x$munsell <- factor(x$munsell, levels = m$munsell[h$order])
x$col <- as.numeric(x$munsell)

m <- m[h$order, , drop = FALSE]
m$col <- parseMunsell(m$m)

soilPalette(m$col, lab = m$munsell)


tm <- voronoiTreemap(
  data = x,
  levels = c('munsell'), sort = FALSE, 
  positioning = 'clustered_by_area'
)

drawTreemap(tm, border_size = 2, color_type = 'categorical', color_palette = m$col)





data("OSDexamples")

x <- OSDexamples$SPC

x$genhz <- generalize.hz(x$hzname, new = c('A', 'B', 'C'), pattern = c('A', 'B', 'C'))

x <- as.data.frame(x)
x <- subset(x, subset = !is.na(hue) & !is.na(value) & !is.na(chroma))

x$munsell <- sprintf('%s %s/%s', x$hue, x$value, x$chroma)

x$munsell <- factor(x$munsell)
x$col <- as.numeric(x$munsell)

ll <- levels(x$munsell)
cols <- data.frame(id = seq_along(ll), col = parseMunsell(ll))


tm <- voronoiTreemap(
  data = x,
  levels = c('munsell'), sort = FALSE, 
  custom_color = 'col', 
  positioning = 'clustered'
)

drawTreemap(tm, border_size = 2, color_type = 'custom_color', color_palette = cols$col, label_size = 3)
