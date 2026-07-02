# Compiles `canny_tudor.tex` and opens the PDF in a browser.
setwd(file.path(here::here(), "man"))
utils::browseURL(tinytex::xelatex("canny_tudor.tex"))
