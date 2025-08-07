##
##
##

# https://en.wikipedia.org/wiki/Fehu

# https://junicode.sourceforge.io/

# https://github.com/psb1558/Junicode-font

# https://www.tidyverse.org/blog/2025/05/fonts-in-r/

library(grid)
library(systemfonts)

typefaces <- system_fonts()[, c("path", "index", "family", "style")]
typefaces[typefaces$family == "Junicode", ]

glyph_info("j", family = "Junicode", size = 30)


grid.newpage()


grid.text(
  "\u16A0\u16A0",
  gp = gpar(fontfamily = "Junicode", fontface = 1, fontsize = 160, bg = 'black')
)

grid.newpage()
vp <- viewport(width = 1, height = 1)

grid.draw(
  gTree(
    children = gList(
      rectGrob(gp = gpar(fill="black")),
      textGrob(
        "\u16A0\u16A0",
        gp = gpar(fontfamily = "Junicode", fontface = 1, fontsize = 360, col = '#AA0000'),
        just = 'center'
      )
    ),
    # gp = gpar(col="white"), 
    vp = vp
  )
)

grid.newpage()
vp <- viewport(width = 1, height = 1)

grid.draw(
  gTree(
    children = gList(
      rectGrob(gp = gpar(fill="white")),
      textGrob(
        "\u16A0\u16A0",
        gp = gpar(fontfamily = "Junicode", fontface = 1, fontsize = 360, col = 'black'),
        just = 'center'
      )
    ),
    # gp = gpar(col="white"), 
    vp = vp
  )
)


