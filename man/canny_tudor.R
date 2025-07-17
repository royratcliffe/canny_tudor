# Compile the LaTeX document using `xelatex` from the `tinytex` package. This
# command will generate the PDF output from the `tex` file. Ensure that the
# `tinytex` package is installed and configured in your R environment. This
# command is typically run in an R script or console. Make sure to have the
# `canny_tudor.tex` file in the working directory before running this command.
# You can run this command in R to compile the LaTeX document.
tinytex::xelatex("canny_tudor.tex")
