
## idea: generate faces for all taxa at top-n levels of ST
## --> ordered factors for order/suborder/greatgroup/subgroup
## --> regular factors (?) for family criteria
##
##

## idea: fill un-used characterization "slots" with 1s




library(aqp)
library(soilDB)
library(sharpshootR)
library(cluster)
library(aplpack)
library(SoilTaxonomy)
library(reshape2)

# photogenic soil series
s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA',  'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')

# subset for Chernoff faces
s.list <- c('fresno', 'redding', 'san joaquin', 'sierra', 'hanford', 'dylan', 'zook', 'lucy', 'pierre')


# get these soil series
s <- fetchOSD(s.list)

# manually convert Munsell -> sRGB
rgb.data <- munsell2rgb(s$hue, s$value, s$chroma, return_triplets = TRUE)
s$r <- rgb.data$r
s$g <- rgb.data$g
s$b <- rgb.data$b

# generate color signature
pig <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'pam', pam.k = 3)

# propagate IDs
# this will be abstracted in the near future
row.names(pig) <- pig[, 1]

# distance matrix no standardization needed
d <- daisy(pig[, -1], )

# divisive hierarchical clustering
dd <- diana(d)


# Chernoff via color signatures
faces(pig[, -1], fill = TRUE)

# order based on clustering
faces(pig[dd$order, -1], fill = TRUE)


# make faces in this order, do not plot
a <- faces(pig[dd$order, -1], plot.faces = FALSE, fill = TRUE)

# add faces above profiles, same order
par(mar = c(0, 0, 2, 0))
plotSPC(s, plot.order = dd$order, name = NA, plot.depth.axis = FALSE, width = 0.35, y.offset = 35)
plot(a, x.pos = 1:length(s), y.pos = rep(-5, times = length(s)), width = 1.25, height = -55, labels = NA, face.type = 0)


data("ST_unique_list")
data("ST")


s$soilorder <- factor(s$soilorder, levels = ST_unique_list$order)
s$suborder <- factor(s$suborder, levels = ST_unique_list$suborder)
s$greatgroup <- factor(s$greatgroup, levels = ST_unique_list$greatgroup)
s$subgroup <- factor(s$subgroup, levels = ST_unique_list$subgroup)


m <- site(s)[, c('soilorder', 'suborder', 'greatgroup', 'subgroup')]

m <- data.frame(lapply(m, as.numeric))
row.names(m) <- profile_id(s)

ST.d <- SoilTaxonomyDendrogram(s, name = NA, width = 0.3, plot.depth.axis = FALSE)

a <- faces(m[ST.d$order, ], plot.faces = FALSE, fill = TRUE)


par(mar = c(0, 0, 2, 0))
plotSPC(s, plot.order = ST.d$order, name = NA, plot.depth.axis = FALSE, width = 0.35, y.offset = 35)
plot(a, x.pos = 1:length(s), y.pos = rep(0, times = length(s)), width = 1.25, height = -55, labels = NA, face.type = 0)




###

# nm <- SDA_query("SELECT DISTINCT mrulename FROM cointerp WHERE seqnum = 0 ;")


# set list of component names, same as soil color example
s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 
            'hanford', 'cecil', 'sycamore', 'zook')

# get OSD details
s <- fetchOSD(soils = s.list)

# set list of relevant interpretations
interp.list <- c(
  'ENG - Construction Materials; Topsoil', 
  'ENG - Sewage Lagoons', 'ENG - Septic Tank Absorption Fields', 
  'ENG - Unpaved Local Roads and Streets',
  'ENG - Dwellings With Basements',
  'DHS - Catastrophic Mortality, Large Animal Disposal, Trench',
  'DHS - Potential for Radioactive Sequestration',
  'AWM - Irrigation Disposal of Wastewater'
)

# compose query
q <- paste0("SELECT UPPER(compname) as compname, mrulename, AVG(interplr) as interplr_mean
FROM component INNER JOIN cointerp ON component.cokey = cointerp.cokey
WHERE UPPER(compname) IN ", toupper(format_SQL_in_statement(s.list)), "
AND seqnum = 0
AND mrulename IN ", format_SQL_in_statement(interp.list), "
AND interplr IS NOT NULL
GROUP BY compname, mrulename;")

# send query
x <- SDA_query(q)

# x$interplr_mean <- 1 - x$interplr_mean

x.wide <- dcast(x, compname ~ mrulename, value.var = 'interplr_mean')

# note: component name and series name have been converted to upper case
# sort rows of fuzzy ratings based on profiles from OSDs
new.order <- match(x.wide$compname, profile_id(s))
x.wide <- x.wide[new.order, ]

# copy ids to row.names so that they are preserved in distance matrix
row.names(x.wide) <- x.wide$compname

# create distance matrix
d <- daisy(x.wide[, -1])

# divisive hierarchical clustering
clust <- diana(d)


par(mar=c(2,0,2,0))
plotProfileDendrogram(s, clust, dend.y.scale = 2, scaling.factor = 0.007, y.offset = 0.15, width=0.3, cex.names = 0.55, name.style = 'center-center', axis.line.offset = -3)

title('Component Similarity via Select Fuzzy Ratings')
mtext('Profile Sketches are from OSDs', 1)


a <- faces(x.wide[clust$order, 3, drop = FALSE], plot.faces = FALSE, fill = TRUE)

par(mar = c(0, 0, 2, 0))
plotSPC(s, plot.order = clust$order, name = NA, plot.depth.axis = FALSE, width = 0.35, y.offset = 35)
plot(a, x.pos = 1:length(s), y.pos = rep(0, times = length(s)), width = 1.25, height = -55, labels = NA, face.type = 1)


a <- faces(x.wide[clust$order, -1, drop = FALSE], plot.faces = FALSE, fill = TRUE)

par(mar = c(0, 0, 2, 0))
plotSPC(s, plot.order = clust$order, name = NA, plot.depth.axis = FALSE, width = 0.35, y.offset = 35)
plot(a, x.pos = 1:length(s), y.pos = rep(0, times = length(s)), width = 1.25, height = -55, labels = NA, face.type = 1)



par(mar=c(0,0,2,0))
plotProfileDendrogram(s, clust, dend.y.scale = 3, scaling.factor = 0.007, y.offset = 0.5, width=0.3, cex.names = 0.55, name.style = 'center-center', axis.line.offset = -3)


plot(a, x.pos = 1:length(s), y.pos = rep(1, times = length(s)), width = 0.8, height = -0.75/2, labels = NA, face.type = 1)

title('Component Similarity via Select Fuzzy Ratings')
mtext('Profile Sketches are from OSDs', side = 1, line = -1.5)

