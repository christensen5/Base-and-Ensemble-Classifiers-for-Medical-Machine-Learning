Convert2NumVect <- function(filein){

stopifnot(is.character(filein))

filepath <- paste0("~/Dropbox/Documents/University/URSS/R/",filein)

temp <- read.table(filepath)
dum = data.matrix(temp)

varout = dum[,2]

return(varout)

}