
# https://cran.r-project.org/web/packages/gghilbertstrings/readme/README.html

# https://github.com/jokergoo/HilbertCurve
# https://bioconductor.org/packages/devel/bioc/vignettes/HilbertCurve/inst/doc/HilbertCurve.html

library(HilbertVis)
library(HilbertCurve)
library(IRanges)


x <- HilbertCurve(1, 100, reference = TRUE, level = 4, mode = 'normal')
hc_segments(x, IRanges(1, 10)) 

x <- HilbertCurve(1, 100, level = 8, mode = 'normal')
hc_segments(x, IRanges(1, 100), gp = gpar(lwd = 0.25)) 

svglite::svglite('hb-5.svg', width = 4, height = 4)

x <- HilbertCurve(1, 100, level = 5, mode = 'normal')
hc_segments(x, IRanges(1, 100), gp = gpar(lwd = 0.5)) 

dev.off()

.n <- 100
col <- hcl.colors(n = .n, palette = 'zissou1')
hc <- HilbertCurve(1, .n, level = 5)
hc_rect(hc, x1 = 1:(.n - 1), x2 = 2:.n, gp = gpar(col = col, fill = col))
hc_segments(hc, IRanges(1, .n), gp = gpar(lwd = 0.5)) 
