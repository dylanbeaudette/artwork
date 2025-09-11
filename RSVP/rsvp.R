
## RSVP: really secure very primitive
## FEHU: Fancy Encryption for Human Use


# https://stackoverflow.com/questions/6791798/convert-string-to-hexadecimal-on-command-line
# https://stackoverflow.com/questions/5806308/how-do-i-encrypt-data-in-r

# consider forward error correction
# https://github.com/coolbutuseless/feck
# seems to crash with very short input
# library(feck)

# low-level image manipulation / 
# https://github.com/richfitz/stegasaur

# TODO: map bytes to RGBA vectors
#       output as PNG
# -> need the PNG header + image geometry encoding

# library(magick)
# 
# i <- image_blank(2, 1, "green")
# r <- image_write(i)
# image_read(r)


# TODO: map bytes -> first 256 kanji (possibly from specific JLPT level)
# matrix(as.raw(0:255), ncol = 16, byrow = TRUE)

# https://github.com/cyphar/heisig-rtk-index/tree/master
# https://github.com/sdcr/heisig-kanjis/blob/master/heisig-kanjis.csv
# k <- read.csv('https://github.com/sdcr/heisig-kanjis/raw/refs/heads/master/heisig-kanjis.csv')
# head(k)


library(stringi)
library(openssl)

source('local-functions.R')


# no compression
# encode -> format
cat(e <- rsvp_e('all work and no play makes jack a dull boy', key = 'ASDF', compress = 'none'))
rsvp_d(e, key = 'ASDF', compress = 'none')


# overhead not worth the compression for short messages
# gzip -> encode -> format
cat(e <- rsvp_e('all work and no play makes jack a dull boy', key = 'ASDF', compress = 'gzip'))
rsvp_d(e, key = 'ASDF', compress = 'gzip')


# compare bloat factor for trivial compression success
# 3.6
cat(e <- rsvp_e('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', key = 'ASDF', compress = 'none'))
# 1.4
cat(e <- rsvp_e('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', key = 'ASDF', compress = 'gzip'))

msg <- paste(as.character(sample(1:255, size = 1e3, replace = TRUE)), collapse = '')
e <- rsvp_e(msg, key = 'ASDF', compress = 'gzip')
attr(e, 'bloatfactor')

# minimum cyphertext length is 79
# cyphertext is always different
cat(e <- rsvp_e('a', key = 'ASDF', compress = 'none'))
cat(e <- rsvp_e('a', key = 'ASDF', compress = 'gzip'))

# other formatting
cat(e <- rsvp_e('a funny little story about nothing', key = 'ASDF', compress = 'none', blocksize = 6, ncolumn = 3))
cat(e <- rsvp_e('a funny little story about nothing', key = 'ASDF', compress = 'none', blocksize = 2, ncolumn = 8))




