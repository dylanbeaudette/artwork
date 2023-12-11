library(fawkes)

# https://axidraw.com/doc/py_api/#quick-start-plotting-svg

## installed with pip from windows command line
# 'c:/Users/Dylan.Beaudette/Application Data/Python/Python310/site-packages/axicli/axidraw_cli.py'

axi_version()

Run `reticulate::py_last_error()` for details.
Error in `import_axidraw()`:
  ! pyaxidraw could not be loaded.
Make sure you have installed the library using `install_axidraw()`

# fawkes:::import_axidraw()


# seems to work...
library(reticulate)
a <- import('axicli')
