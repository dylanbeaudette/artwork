library(terra)

x <- vect('e:/personal/GPX/06-Apr-2024-1119.gpx', layer = 'track_points')

plot(x)
x <- project(x, 'epsg:32610')

d <- distance(x, sequential = TRUE)
ts <- strptime(x$time, format = '%Y/%m/%d %H:%M:%S')

plot(ts, d, type = 'l', las = 1)

length(d)
length(ts)

# m/s
v <- d[-1] / as.numeric(diff(ts))

# m/s -> mph
v <- v * 2.23694

hist(v)

plot(ts[-1], d[-1], type = 'l', las = 1)


plot(ts[-1], d[-1], type = 'p', las = 1, cex = sqrt(v) * 0.5)


