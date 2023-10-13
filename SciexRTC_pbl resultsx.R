#' Generates the corrected retention time (RTC) from the initial pooled sample
#' injection based on the observed retention time of the heavy ions for each
#' Component Group (i.e. peptide sequence).
#' 
#' NOTE: the order of the Components is important to preserve because the RTC
#' values will be directly copied into the Analyst method
#' 
#' Input file: MultiQuant / SciexOS text file output containing component ID
#' and observed retention time for all analytes in the method.
#' 
#' Note: file paths must use the the '/' separator instead of the Windows default '\'

library(tidyverse)
library(dplyr)
library(stringr)

input_file <- "C:/Users/phamh2/Documents/R/Examples/20220323_MD_Boron_RTC.txt"
output_file <- "C:/Users/phamh2/Documents/R/Examples/02.csv"

RTC <- data %>% 
#  filter(grepl("\\.heavy$", `Component Name`)) %>%
  filter(grepl("heavy", `Component Name`)) %>%  
#  select(`Component Group Name`, `Retention Time`) %>% 
  group_by(`Component Group Name`)  %>%
  summarise(RTC = mean(`Retention Time`))

 data_RTC <- semi_join(data, RTC, by="Component Group Name")
 view (data_RTC)
# uncomment to export only certain columns
# data_RTC <- select(data_RTC, `Component Name`, `Component Group Name`, `Retention Time`, `RTC`)

#if (file.exists(output_file)) {
#  stop("Specified output_file already exists! Did not save RTC.")
#} else {
#  write_csv(data_RTC, output_file)
#}

