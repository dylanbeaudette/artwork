library(aqp)
library(rayrender)
library(viridisLite)
library(scales)

.n <- 500
.x <- runif(.n, min = -2, max = 2)
.y <- runif(.n, min = -2, max = 2)
.z <- runif(.n, min = -5, max = 2)
.r <- pmax(0.05, rnorm(.n, mean = 0.1, sd = 0.1))


# .g <- expand.grid(
#   x = seq(from = -2, to = 2, length.out = 6),
#   y = seq(from = -2, to = 2, length.out = 6),
#   z = seq(from = -5, to = 2, length.out = 6)
# )
# 
# .x <- jitter(.g$x)
# .y <- jitter(.g$y)
# .z <- jitter(.g$z)
# .r <- 0.1
# .n <- nrow(.g)


.cp <- mako(10)
.col <- col_numeric(.cp, domain = range(.z))


for(i in 1:.n) {

  .material <- dielectric(color = .col(.z[i]), refraction = 0.1)
  .sphere <- sphere(radius = .r[i], x = .x[i], y = .y[i], z = .z[i], material = .material)
  
  if(i == 1) {
    s <- add_object(.sphere)  
  }
  
  s <- add_object(s, .sphere)
  
}


render_scene(s, parallel = TRUE, width = 800, height = 800, samples = 100)


## next time CSG
# 
# generate_ground(material=diffuse(checkercolor="grey20")) %>% 
#   add_object(csg_object(csg_combine(
#     csg_sphere(x=-0.4,z=-0.4),
#     csg_sphere(x=0.4,z=0.4), operation="subtractblend"),
#     material=glossy(color="dodgerblue4"))) %>%
#   add_object(sphere(y=5,x=5,radius=3,material=light(intensity=10))) %>%  
#   render_scene(clamp_value=10,fov=20,lookfrom=c(-3,5,10))
# 
# 
# 
