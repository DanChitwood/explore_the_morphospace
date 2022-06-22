#This is a script to convert rows of Procrustes coordinates into a tabular format

data <- read.table("./5.Procrustes_coord_all.txt", header=TRUE)

#Returns the names of the columns
names(data)

#What is the total length of the dataset?
len <- length(data$order)

#Let's make a table to put our new data into. The new table will need enough rows to accomodate each leaf (length of the dataset/18) and 18 columns for the 9 coordinates
overall.table <- matrix(nrow=len/18, ncol=(9*2))

#Let's calculate how many interations the loop will have to iterate (the same as the number of leaves, total length/18)
overall.length <- len/18

#Let's make a loop to reformat our data. It will iterate, as a count of j, from 1 to the total number of leaves
for(j in c(1:overall.length)) {

#Let's keep track of where we are in the loop by printing j with each iteration	
print(j)

#First, let's extract x,y coordinates from the original table as blocks of 42 rows, each representing a leaf, one by one, as calculated from j.	
sub.data <- as.matrix(data[ (1+18*(j-1)):((1+18*(j-1))+17), 2])

#Let's dissect out each landmark of 42 rows and put it into sequential columns of a single row (for a single leaf) in the overall table

#x coordinates
overall.table[j,1] <- sub.data[1, 1]
overall.table[j,3] <- sub.data[2, 1]
overall.table[j,5] <- sub.data[3, 1]
overall.table[j,7] <- sub.data[4, 1]
overall.table[j,9] <- sub.data[5, 1]
overall.table[j,11] <- sub.data[6, 1]
overall.table[j,13] <- sub.data[7, 1]
overall.table[j,15] <- sub.data[8, 1]
overall.table[j,17] <- sub.data[9, 1]

#y coordinates
overall.table[j,2] <- sub.data[10, 1]
overall.table[j,4] <- sub.data[11, 1]
overall.table[j,6] <- sub.data[12, 1]
overall.table[j,8] <- sub.data[13, 1]
overall.table[j,10] <- sub.data[14, 1]
overall.table[j,12] <- sub.data[15, 1]
overall.table[j,14] <- sub.data[16, 1]
overall.table[j,16] <- sub.data[17, 1]
overall.table[j,18] <- sub.data[18, 1]


#Once that is done, repeat! for the next leaf until finished

}

#Let's double check the loop worked
head(overall.table)
tail(overall.table)

#Let's give the new table some column names
colnames(overall.table) <- c("xA", "yA", "xB", "yB", "xC", "yC", "xD", "yD", "xE", "yE", "xF", "yF", "xG", "yG", "xH", "yH", "xI", "yI")

#################
#################
#################

#Let's read in the original data to calculate more variables and merge with procrustes adjusted coordinates

data_info <- read.table("./2.passiflora_grapevine.txt", header=TRUE)
head(data_info)
tail(data_info)
names(data_info)

################
# calculate leaf area
################

attach(data_info)

data_info$leaf_area <- (0.5*abs(

(xA*yE + xE*yF + xF*yG + xG*yH + xH*yI + xI*yD + xD*yA) - 

(yA*xE + yE*xF + yF*xG + yG*xH + yH*xI + yI*xD + yD*xA ) 

))

detach(data_info)

#######################
# calculate CONVEX area
#######################

attach(data_info)

data_info$convex_area <- (0.5*abs(

(xA*yE + xE*yG + xG*yI + xI*yD + xD*yA) - 

(yA*xE + yE*xG + yG*xI + yI*xD + yD*xA) 

))

detach(data_info)

#######################
# calculate vein area
#######################

attach(data_info)

data_info$vein <- (0.5*abs(

(xA*yE + xE*yB + xB*yC + xC*yI + xI*yD + xD*yA) - 

(yA*xE + yE*xB + yB*xC + yC*xI + yI*xD + yD*xA) 

))

detach(data_info)

#######################
# calculate values
#######################

data_info$vein_to_blade_area <- log(data_info$vein / (data_info$leaf_area-data_info$vein))
data_info$vein_to_blade_convex <- log(data_info$vein / (data_info$convex_area-data_info$vein))
data_info$solidity <- data_info$leaf_area / data_info$convex_area

names(data_info)

# combine with procrustes coordinates

meta_data <- data_info[c(1:9,31:33)]

head(meta_data)

final_table <- cbind(meta_data, overall.table)
names(final_table)
head(final_table)

#Let's write out the table
write.table(final_table, "7.Procrustes_reformat_all.txt")





