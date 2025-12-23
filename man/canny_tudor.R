# Compile the LaTeX document using `xelatex` from the `tinytex` package. This
# command will generate the PDF output from the `tex` file. Ensure that the
# `tinytex` package is installed and configured in your R environment. This
# command is typically run in an R script or console. Make sure to have the
# `canny_tudor.tex` file in the working directory before running this command.
# You can run this command in R to compile the LaTeX document.
#
# Open the generated PDF in a web browser using the `browseURL` function from
# the `utils` package. This will allow you to view the PDF output directly in
# your default web browser. The `tinytex::xelatex` function raises an error if
# the compilation fails.
#
# Set the working directory to the location of the current R script. This is
# useful if you are running this code in an R script or R Markdown document.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
utils::browseURL(tinytex::xelatex("canny_tudor.tex"))
