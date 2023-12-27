
# https://www.frontiersin.org/articles/10.3389/fpsyg.2021.742577/full
# https://github.com/nhtran93/kolam

library(kolam)


plotLoop(c("o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2",
           "o1", "o3", "o4", "o1", "o1","o3", "o4", "o1", "o1", "o2"),
         xStart = 0, yStart = 0.5, headingStart = 45, 
         arrow = FALSE)


plotLoop(c("c1"), xStart = -1, yStart = -0.5,
         headingStart = 45, add = TRUE, col = "blue", 
         arrow = FALSE)


g <- rep(c('o1', 'o2', 'o1', 'o4', 'o1'), times = 8)
k <- plotLoop(g, xStart = 0, yStart = 0.5, headingStart = 45, arrow = FALSE)

g <- rep(c('o1', 'o3', 'o4', 'o1'), times = 4)
k <- plotLoop(g, xStart = 1.5, yStart = 1, headingStart = 45, arrow = FALSE, add = TRUE)



g <- rep(c('d4r', 'd4r', 'd4r', 'd2r', 'd2r'), times = 4)
k <- plotLoop(g, xStart = 0.5, yStart = 0.5, headingStart = 90, arrow = FALSE)


gen <- function(iter = 100) {
  .o <- c('o1', 'o2', 'o3', 'o4')
  .t <- c('t1', 't2', 't3', 't4')
  .d <- c()
  
  g.o <- sample(.o, size = iter, replace = TRUE)
  g.t <- sample(.t, size = 1)
  
  
  
}

g <- gen()
k <- plotLoop(g, xStart = 0, yStart = 0.5, headingStart = 45)



plotKolam(kolamObject[[1]])

plotKolam(kolamObject[[2]], arrow = FALSE, cex = 0.4)

# Turn off all the loop colouring
plotKolam(kolamObject[[4]], arrow = FALSE, cex = 0.4, loop_col = FALSE)

