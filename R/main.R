
#set ./R directory as working directory
#note: in windows you might chance the path name, (i.e \\ in place of /)

## this can be sourced once----
# contruct dataframe
source("ComputeDistance.R")

#weird struff to do because of the way filenames are stored in the main dataframe
data_dir="../AG2_ramp/DATI/"
setwd(data_dir)
source("../../R/CreateComplete_df.R")
##

#results plot----

#test model
source("../../R/testModel.R")

#probability contour
source("../../R/probabilityContour.R")


# once you create the main dataframe you may also copy it in the ./R directory
# and set it as working directory to run the other script