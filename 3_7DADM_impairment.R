# load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

# #base folder, the rest of the folders are derived from this base folder
# base_loc <-"D:\\Hoh tribe Bray Water Quality\\Water quality monitoring program\\Continuous temperature\\Data\\"
# 
# # to handle data from a new water year, just change these file paths
# fall_data_loc = paste0(base_loc, "2023\\2023 fall\\4. Finalized data\\")
# sum_data_loc = paste0(base_loc, "2024\\2024 summer\\4. Finalized Data\\")
# lookup_loc = paste0(base_loc, "lookup_tables\\")

# # Jess filepaths for 2022-2023 data from Kim (time zone worked)
# base_loc = "~/Documents/TribalConsulting/HohTribe/Bray Hoh data 3.29.24/"
# fall_data_loc <- paste0(base_loc, "2022/2022 fall/")
# sum_data_loc <- paste0(base_loc, "2023/2023 summer/")
# plots_loc <- paste0(base_loc, "plots/")
# lookup_loc = base_loc

# Jess filepaths for 2023-2024 data from Kim (need to update handling of time zone)
base_loc = "/Users/jessicakunke/Documents/TribalConsulting/HohTribe/HohTribe_code_June2025/"
fall_data_loc <- paste0(base_loc, "2023_fall_data/")
sum_data_loc <- paste0(base_loc, "2024_summer_data/")
plots_loc <- paste0(base_loc, "plots/")
lookup_loc = base_loc

# read in site-group lookup table
# - For each site, which WQS group is it in?
site_group = read_csv(paste0(lookup_loc, "site_group.csv")) %>%
  # removes any completely empty rows (in case this data frame is written
  # in Excel in the future and Excel adds extra empty lines)
  filter_all(any_vars(!is.na(.)))

# read in group-WQS lookup table
# - For each group, how does the WQS change over the water year?
group_wqs = read_csv(paste0(lookup_loc, "group_wqs.csv")) %>%
  # removes any completely empty rows (in case this data frame is written
  # in Excel in the future and Excel adds extra empty lines)
  filter_all(any_vars(!is.na(.)))

# read in quality-controlled data ----------------------

cat("Reading in QC'd data...", fill = TRUE)
# assumes all data files are csv format, not xls or xlsx
# read in fall deployment files
# fall filename format: "AndersonLower_2023_fall_FINAL.csv"
data_files_fall = list.files(path = fall_data_loc, pattern = '*csv')
# read in summer deployment files
# summer filename format: "AndersonLower_2024_sum_FINAL.csv"
data_files_sum = list.files(path = sum_data_loc, pattern = '*csv')
# combine fall and summer filenames into one list so we just need one loop
data_files = c(data_files_fall, data_files_sum)

not_Jims_Well = unlist(lapply(data_files, function(x) !startsWith(x, "JimsWell")))
data_files = data_files[not_Jims_Well]

# loop through each file, read it in, process it
all_data <-NULL # Create new data frame to hold combined data

file_no = 0
for(this_file in data_files){ # for each file,
  # update file_no counter
  file_no = file_no + 1
  # this_file = data_files[1] # uncomment to troubleshoot within loop
  # this_file = "EFKalaloch_2023_fall_FINAL.csv"
  
  # extract metadata (sitename and deployment season) from filename
  filename_parts = strsplit(this_file, '[_.]')[[1]]
  sitename = filename_parts[1]
  deploy_season = paste(filename_parts[3], filename_parts[2])
  
  # all the fall files are first in the list, so we can use this to tell
  # from file_no whether the current file is a summer or fall deployment
  # and that tells us in which directory we can find this file
  if(file_no > length(data_files_fall)){
    file_dir = sum_data_loc
  }else{
    file_dir = fall_data_loc
  }
  
  # read in the data (this_file) from the right directory (file_dir)
  this_data = readr::read_csv(file = paste0(file_dir, this_file),
                              skip = 1, # skip header line
                              # the only columns we really need are 2, 3, 10, and 13,
                              # (date, time, water temp, and the QC indicator UseForCalc)
                              # but we can keep them all for reference
                              col_select = c(2, 3, 10, 13), # read only the four columns of data we need
                              col_names = FALSE, # don't try to name columns from a row of the file
                              show_col_types = FALSE) %>% # suppresses print message
    data.frame() %>% # get rid of annoying attribute information
    # the next several lines convert the character-format datetime to an R POSIXct object
    # ymd_hms is the format the character string is in initially; it tells R
    # how to read and interpret the character string
    dplyr::rename(
      Date = X2,
      Time = X3,
      WaterTemp = X10,
      UseForCalc = X13) %>%
    dplyr::mutate(
      Date = lubridate::mdy(Date),
      Time = as.character(Time),
      SiteName = sitename,
      DeploySeason = deploy_season) %>%
    # keep only the data with UseForCalc = 1 (data that passed QC)
    dplyr::filter(UseForCalc == 1)
  
  # add this file's data to the combined data frame
  all_data <- dplyr::bind_rows(all_data, this_data)
  # print some basic info to the R Console to update us on what was read in
  cat(paste0(" - Site ", sitename, " Deployment ", deploy_season,
             " Dates ", min(this_data$Date), " to ", max(this_data$Date)),
      fill = TRUE)
} # end for-loop
cat("Done reading in QC'd data.", fill = TRUE)

# compute 7DADM for this data ---------------------------

cat("Computing 7DADM...", fill = TRUE)
temp_stats = all_data %>%
  # compute daily max water temp by site and date
  dplyr::group_by(SiteName, Date)  %>%
  dplyr::summarize(DailyMax = max(WaterTemp, na.rm = TRUE), .groups = 'drop') %>%
  # add in any missing SiteName-Date combinations so we can tell which data is
  # for consecutive dates
  dplyr::right_join(data.frame(SiteName = rep(unique(all_data$SiteName),
                                              each = as.integer(max(all_data$Date)-min(all_data$Date))+1),
                               Date = rep(seq(min(all_data$Date), max(all_data$Date), 1),
                                          dplyr::n_distinct(all_data$SiteName))),
                    by = dplyr::join_by(SiteName, Date)) %>%
  # sort by SiteName first, then by Date within SiteName
  dplyr::arrange(SiteName, Date) %>%
  # compute 7DADM
  dplyr::group_by(SiteName) %>%
  dplyr::mutate(sevenDADM = runner::mean_run(x = DailyMax,
                                             k = 7, lag = -3,
                                             idx = Date,
                                             na_pad = TRUE, na_rm = FALSE)) %>%
  dplyr::ungroup()

cat("Done computing 7DADM.", fill = TRUE)

# use site-group lookup table to determine which WQS group each site is in -----

site_group = distinct(site_group) # make sure only unique rows
temp_stats$SiteName[temp_stats$SiteName == "HohONPBoundary"] = "HohONP"

temp_stats = dplyr::left_join(
  temp_stats, site_group,
  by = dplyr::join_by(SiteName == site),
  relationship = "many-to-one")

# use group-WQS lookup table to determine what the WQS is for each group ------
# over the course of the water year, and determine when exceedances happen
# match_group_to_WQS(group_wqs)
temp_stats = temp_stats %>%
  dplyr::mutate(Day = format(base::as.Date(Date), "%m/%d")) %>%
  dplyr::left_join(group_wqs, by = "grp_type", relationship = "many-to-many") %>%
  dplyr::mutate(dates_match = (Day > start_date & Day < end_date)) %>%
  dplyr::filter(dates_match) %>%
  # dplyr::rename(WQS = wqs) %>%
  dplyr::select(SiteName, Date, Day, sevenDADM, WQS) %>%
  dplyr::mutate(Exceedance = (sevenDADM > WQS))


# generate a table of site impairments ---------------------
# site_impairments = site_impairments_table(wq_data)

site_impairments = data.frame(
  Site = character(),
  Impaired = character(),
  Dates = character())

# format Dates to have date ranges for consecutive dates,
# e.g. 7/18-7/20 instead of 7-18, 7-19, 7-20
for(site in unique(temp_stats$SiteName)){
  # cat(site, fill = TRUE)
  exceedance_dates = dplyr::filter(temp_stats, SiteName == site, Exceedance)$Date
  diffs = diff(exceedance_dates)
  if(length(diffs) >=1 & sum(diffs>7) > 0){
    # cat(site, fill = TRUE)
    consec_dates = split(format(exceedance_dates, "%m/%d"),
                         cumsum(c(1, diff(exceedance_dates) != 1)))
    date_list = sapply(consec_dates, function(x) paste(x[1], tail(x, n=1), sep = "-"))
    site_impairments = dplyr::add_row(site_impairments,
                                      Site = site,
                                      Impaired = "Yes",
                                      Dates = paste(date_list, collapse = ", "))
  } else {
    # cat(site, fill = TRUE)
    site_impairments = dplyr::add_row(site_impairments,
                                      Site = site,
                                      Impaired = "No",
                                      Dates = NA)
  }
}


