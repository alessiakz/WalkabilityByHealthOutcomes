#setting root directory
here::i_am("code/04_render.R")

#loading packages
library(rmarkdown)

#rendering report
render("Report.Rmd")
