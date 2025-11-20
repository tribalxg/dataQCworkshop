# load necessary libraries
library(readxl)
# all the following six libraries can be loaded either in a single line using
# library(tidyverse)
# or separately as below
library(lubridate) # for handling dates
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# base folder
base_loc <- "D:\\Hoh tribe Bray Water Quality\\Water quality monitoring program\\Continuous temperature\\Data\\2022\\2022 summer_NEW\\2. Cropped files\\"

# input folder; directory containing your raw data
rawdata_loc <- paste0(base_loc, "1. Raw csv\\")

# output folder; directory where your cropped data will be (or currently is) stored
cropped_loc <- paste0(base_loc, "2. Cropped csv\\")

# also an output folder; directory where plots of cropped and raw data will be stored
croppedplots_loc <- paste0(base_loc, "3. Cropped plots\\")

# name of your LDRTimes file that has the lookup table with deployment/retrieval dates
# assumes ldrtimes_fn is in rawdata_loc folder
ldrtimes_fn <- "LDRTimes_summer22.xlsx"

# -------------------------------------------
# editing mostly takes place above this line
# -------------------------------------------

# check that R can find your raw data files
# Get all temperature data filenames
# note: * is called a glob, short for global
# IMPORTANT: filenames must be in the format sitename_medium_deployseason_deployyear.csv
#            e.g. NolanLower_air_sum_23.csv
csv_files = list.files(path = rawdata_loc, pattern = '*csv')
# this is now a list of all filenames; we haven't read in the data yet, but
# make sure this lists all the raw files you want to crop
csv_files

# read in LDR file and take a look at it
# note: this assumes the LDR file is in the folder indicated by rawdata_loc
ldrtimes = readxl::read_xlsx(paste0(rawdata_loc, ldrtimes_fn))

# once you're sure that file paths are working and your ldrtimes looks right,
# crop the files!

i = 0
for(this.file in csv_files){
  i = i + 1
  #this.file = csv_files[1] # uncomment to troubleshoot within loop
  cat(paste0("Reading file ", i, " of ", length(csv_files), ": ", this.file), fill = TRUE)
  
  # extract metadata from the filename
  filename.parts = stringr::str_split_1(this.file, '[_.]')
  csv.site = filename.parts[1]
  csv.media = filename.parts[2]
  csv.season = filename.parts[3]
  csv.year = filename.parts[4]
  
  # convert the character-format datetime to an R POSIXct object
  # ymd_hm is the format the character string is in initially; it tells R
  # how to read and interpret the character string
  # sometimes R reads in the datetime format as mdy_hms and sometimes mdy_hm.
  # This tryCatch handles either hh:mm:ss or hh:mm format in csv files
  this.data =  tryCatch(
    {
      readr::read_csv(paste0(rawdata_loc, this.file),
                      skip = 2, # skip the first two lines of the file
                      col_select = 1:3, # read only the first three columns of data
                      col_names = FALSE, # don't try to name columns from a row of the file
                      show_col_types = FALSE) %>% # suppresses print message
        dplyr::rename("row.num" = X1,
                      "datetime" = X2,
                      "temperature" = X3) %>%
        dplyr::mutate(datetime = lubridate::mdy_hms(datetime)) #for datetime in hh:mm:ss
    },
    warning = function(cond) { #if datetime isn't in hh:mm:ss, will now try hh:mm format
      readr::read_csv(paste0(rawdata_loc, this.file),
                      skip = 2, # skip the first two lines of the file
                      col_select = 1:3, # read only the first three columns of data
                      col_names = FALSE, # don't try to name columns from a row of the file
                      show_col_types = FALSE) %>% # suppresses print message
        dplyr::rename("row.num" = X1,
                      "datetime" = X2,
                      "temperature" = X3) %>%
        dplyr::mutate(datetime = lubridate::mdy_hm(datetime)) #for datetime in hh:mm
    }
  )
  
  # crop the data
  deploy.retrieval = ldrtimes %>%
    # select the row(s) of ldrtimes that match this datafile
    # should be exactly one row, but if there are no rows or multiple rows that
    # match, this step will pull that many rows
    dplyr::filter(site == csv.site, deploy_season == csv.season,
                  deploy_year == csv.year, media == csv.media) %>%
    # keep just the deploy_time and retrieval_time variables/columns
    dplyr::select(deploy_time, retrieval_time)
  
  if(nrow(deploy.retrieval) == 0){
    stop("no rows of ldrtimes matched this csv file.")
  }
  if(nrow(deploy.retrieval) > 1){
    stop("multiple rows of ldrtimes matched this csv file.")
  }
  
  deploy = deploy.retrieval$deploy_time
  retrieval = deploy.retrieval$retrieval_time
  
  if(retrieval > deploy) {
    cropped.data = dplyr::filter(this.data,
                                 datetime > deploy,
                                 datetime < retrieval)
    
  } # if(retrieval > deploy)
  
  # write cropped csv files to cropped folder
  readr::write_csv(cropped.data,
                   file=paste0(cropped_loc,
                               stringr::str_split_i(this.file, "[.]", 1), "_cropped.csv"))
  
  #Create a dataframe of the raw and cropped data
  cropvraw <- dplyr::left_join(this.data, cropped.data, by=c("row.num", "datetime")) %>%
    dplyr::rename(raw.temp = temperature.x,
                  cropped.temp = temperature.y) %>%#rename temperature from each file
    #create new column of data type (raw or cropped for plotting in ggplot)
    tidyr::pivot_longer(cols = raw.temp:cropped.temp,
                        names_to="type", values_to="temp")
  
  cropvraw.plot <- ggplot2::ggplot(cropvraw,
                                   ggplot2::aes(x = datetime,
                                                y = temp,
                                                color = type)) +
    ggplot2::geom_line(na.rm=TRUE) +
    ggplot2::geom_point(na.rm=TRUE) +
    ggplot2::labs(title = paste0(" Raw versus Cropped data"),
                  x = "Date", y = "Temperature (C)")+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black", size = (12)))
  
  ggplot2::ggsave(paste0(croppedplots_loc, csv.site, "_", 
                         csv.media, "_rawvscroppeddata.png"),
                  cropvraw.plot,
                  width = 11, height = 8.5, units = "in")
  
}; cat("Done.", fill = TRUE)


