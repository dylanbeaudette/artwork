
## RSVP: really secure very primitive
## FEHU: Fancy Encryption for Human Use


# https://stackoverflow.com/questions/6791798/convert-string-to-hexadecimal-on-command-line
# https://stackoverflow.com/questions/5806308/how-do-i-encrypt-data-in-r

# consider forward error correction
# https://github.com/coolbutuseless/feck


library(stringi)
library(openssl)


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


# txt: crunched raw vector
formatBlocks <- function(txt, bs, nc) {
  
  .n <- nchar(txt)
  .nchunks <- ceiling(.n / bs)
  
  # create from-to sequences for splitting into blocks of size `bs`
  .idx1 <- c(1, 1:.nchunks * bs + 1)
  .idx2 <- 1:.nchunks * bs
  .m <- cbind(
    .idx1[-length(.idx1)], 
    .idx2
  )
  
  # split text into blocks
  .b <- stri_sub(txt, from = .m)
  
  # number of rows required for `nc` columns of `bs` block size
  .nrow <- ceiling(length(.b) / nc)
  .res <- vector(mode = 'character', length = .nrow)
  
  # form columns of blocks
  for(i in seq_len(.nrow)) {
    .s <- i * nc - nc + 1
    .f <- i * nc
    .sub <- .b[.s:.f]
    .sub <- na.omit(.sub)
    .res[i] <- paste(.sub, collapse = ' ')
  }
  
  # smash into single character
  .res <- paste(.res, collapse = '\n')
  
  # sanity check on number of characters before / after
  stopifnot(.n == nchar(gsub('[ \n]', '', .res)))
  
  return(.res)
}

# bl: formatted blocks, character vector of length 1
# convert to raw vector
flattenBlocks <- function(bl) {
  # remove white space and new lines
  .txt <- gsub('[ \n]', '', bl)
  
  # from-to indexes for splitting into 2-character hex bytes
  .n <- nchar(.txt)
  .s <- seq(from = 1, to = .n, by = 2)
  .f <- .s + 1
  .m <- cbind(.s, .f)
  
  # split into hex byte vector
  .b <- stri_sub(.txt, from = .m)
  
  # interpret as raw vector
  .res <- as.raw(sprintf("0x%s", .b))
  
  # TODO: consider extracting IV and saving to attribute
  
  return(.res)
}


# e: raw vector
crunch <- function(e) {
  paste(as.character(e), collapse = '')
}

# blocksize of 4 will always result in complete blocks
rsvp_e <- function(plaintext, key, blocksize = 4, ncolumn = 4, compress = 'none') {
  
  # aes-256 requires 32 byte key
  key <- sha256(charToRaw(key))
  
  # compress = 'none': char => raw
  # compress = 'gzip' etc.: char => raw => compress => raw
  z <- memCompress(plaintext, type = compress)
  
  # encrypt, IV is stored as an attribute
  # cannot decrypt without it
  e <- aes_cbc_encrypt(z, key = key)
  
  # prefix with the 16-byte IV
  e <- as.vector(c(attr(e, 'iv'), e))
  
  # raw vector => character vector (hex bytes) without white space
  txt <- crunch(e)
  
  # format hex byte string => blocks
  bl <- formatBlocks(txt, bs = blocksize, nc = ncolumn)
  
  # diagnostics
  .bloatfactor <- round(nchar(bl) / nchar(plaintext), 1)
  .msg <- sprintf("characters:   %s\nbloat factor: %s", nchar(bl), .bloatfactor)
  message(.msg)
  
  attr(bl, 'bloatfactor') <- .bloatfactor
  
  return(bl)
}

rsvp_d <- function(cyphertext, key, compress = 'none') {
  
  # convert key to 32 byte hash
  key <- sha256(charToRaw(key))
  
  # flatten block text => raw vector
  .d <- flattenBlocks(cyphertext)
  
  # unencrypt -> decompress -> char
  # first 16 bytes are the IV
  # remaining are the cyphertext
  .z <- aes_cbc_decrypt(.d[-(1:16)], key = key, iv = .d[1:16])
  
  .res <- memDecompress(.z, asChar = TRUE, type = compress)
  return(.res)
}

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




