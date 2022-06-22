#require(devtools)
#install_version("shapes", version = "1.1-10", repos = "http://cran.us.r-project.org")

#install.packages("./shape", repos = NULL, type="source")



#Read in the 'shapes' library
library(shapes)

#SPECIFY LANDMARK NUMBER (K), LANDMARK DIMENSIONS (M), and NUMBER OF SAMPLES (N)
#########

k <- 9
m <- 2
n <- 15554

#READ IN DATA WITH ASSOCIATED PARAMETERS
#########

data <- read.in("./3.just_coordinates.txt",k,m)

head(data)


#PERFORM PROCRUSTES ANALYSIS, ALLOW REFLECTION
########

GPA <- procGPA(data, reflect=TRUE)

#LOOK AT VARIOUS OUTPUTS FROM THE ANALYSIS
########

#For help, use ?. Read the returned values, which you can retrieve as below, to understand the output of this function
?procGPA
?shapepca

#LOOK AT THE PROCRUSTES PCS, THE EIGENLEAVES
##########

#How much variance does each PC represent?
GPA$percent

#We can use the default function shapcepca() to "look" at the PCs
shapepca(GPA, pcno=c(1,2,3), joinline=c1,5,6,7,8,9), mag=1)


#write out the Procrustes-adjusted coordinates, which we will discuss at a later date.
###########

pa <- as.matrix(GPA$rotated)
write.table(pa, file="5.Procrustes_coord_all.txt")