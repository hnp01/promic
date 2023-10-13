## the input file is exported from SciexOS. 
## this exported file contains columns as specified by settings files in Haos folder

library(tidyverse)
library(dplyr)

input_file <- "C:/Users/phamh2/Documents/R/Examples/C220503.txt"

data = read_delim(input_file, "\t", na="N/A")

df_1 = data.frame("ISP" = c(1:8),
                  `Component_Group_Name` = c("PBL-QC.QSELSAK","PBL-QC.ATEEQLK","PBL-QC.WVGYGQDSR","PBL-QC.FSVVYAK","PBL-QC.IDPNAWVER","PBL-QC.Gonadtrophin","PBL-QC.GDFQFNISR","PBL-QC.APVLFFDR"),
#                  `Component_Group_Name` = c("QSELSAK","ATEEQLK","WVGYGQDSR","FSVVYAK","IDPNAWVER","Gonadtrophin","GDFQFNISR","APVLFFDR"),
#                  "Intensity_Threshold" = formatC(as.numeric(c(3.0e5,2.0e5,4.0e5,8.0e5,6.0e4,1.0e5,3.0e4,8.0e5)),format="e", digits = 2 ))
                  "Intensity_Threshold" = c(3.0e5,2.0e5,4.0e5,8.0e5,6.0e4,1.0e5,3.0e4,8.0e5 ))

df_2 = data %>%
##  filter(grepl("PBLQC_0?2", `Sample Name`,ignore.case= TRUE)) %>%
  filter(grepl("PBLQC_(.*_)?0?2", `Sample Name`,ignore.case=TRUE)) %>%
  group_by(`Component Group Name`)%>%
#  summarise(Peak_Intensity = formatC(max(`Height`),format="e", digits = 2)) %>%
  summarise(Peak_Intensity = max(`Height`)) %>%
  rename(`Component_Group_Name`=`Component Group Name`)

#df_3 = sub("Component Group Name","",colnames(df_2[1]))
#data_2 = unique(data_1)
intensity = left_join(df_1,df_2,by="Component_Group_Name")

intensity$`Criteria_Met?`= ifelse(intensity$Intensity_Threshold < intensity$Peak_Intensity, "Yes", "No")

view(intensity)
