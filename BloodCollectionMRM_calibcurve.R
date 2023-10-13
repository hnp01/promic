#' The gist of it is we want to compare the signal between different sources of 
#' blood, from intravenous blood draws compared to finger-pricks on Mitra tips, etc. 
#' 
#' Stephen has 8 volunteers, 6 biological replicates each for the two comparison conditions.
#' 
#' These samples have been acquired with the HSP target list + DIA experiments
#' 
#' The comparisons are as follows:
#'  - compare venous blood on Mitra Devices vs capillary blood on Mitra Devices with controls
#'  - three replicates of pool4 plasma
#'  - three replicates of pool of all 8 volunteers venous blood on Mitra Devices
#'  - three replicates of pool of all 8 volunteers capillary blood on Mitra Devices
#'  - three replicates of pool of all 8 volunteers plasma on Mitra Devices.

library(readxl)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(plotly)

data <- list.files("C:/Users/phamh2/Documents/R/Examples/data",
           recursive = T, pattern = "msassay_report.xlsx", full.names = T) %>% 
  lapply(function(filename) {
    tb <- list(read_excel(filename))
    names(tb) <- dirname(filename)
    return(tb)
  }) %>%
  unlist(recursive = F) %>% 
  bind_rows(.id = "ID") %>% 
  extract(col="ID", into = "Job", regex = "(SJOB[0-9]+)") %>% 
  mutate(Plate = ifelse(grepl("SJOB8237|SJOB8238", Job), "Plate1", "Plate2")) %>% 
  extract(col = "sample_name", into = "index", regex = ".*_P[1,2][G,H]([0-9]+)_*", remove = F) %>% 
  mutate(index = as.numeric(index),
         CC_AR = 1 / AreaRatio) %>% 
  mutate(concentration = case_when(
    index==1 ~ 0,
    index==2 ~ 0.3,
    index==3 ~ 0.6,
    index==4 ~ 0.9,
    index==5 ~ 3,
    index==6 ~ 30,
    index==7 ~ 150,
    index==8 ~ 225,
    index==9 ~ 300,
    index==10 ~ 600,
    index==11 ~ 900,
    index==12 ~ 1200
  ))

# write_csv(data, "output/combined_data.csv")

### CALIBRATION CURVE ###

data_calib <- data %>% 
  filter(grepl("^WB_P[1,2]", sample_name)) %>% 
  # filter(component_name == "sp|O14791|APOL1_HUMAN.VAQELEEK.+2y6")
  group_split(component_name)
  # .[4:5]

regression <- data_calib %>% 
  lapply(function(df){
    formula1 <- stats::lm(CC_AR ~ concentration, df[df$Plate=="Plate1",])
    formula2 <- stats::lm(CC_AR ~ concentration, df[df$Plate=="Plate2",])
    
    regression <- rbind(
      c("component_name" = df$component_name[1], "Plate" = 1, formula1$coefficients),
      c("component_name" = df$component_name[1], "Plate" = 2, formula2$coefficients)
    ) %>% 
      as_tibble()
  }) %>% 
  bind_rows() %>% 
  rename("slope" = "concentration")

static <- data_calib %>% 
  lapply(., function(df) {
    p1 <- ggplot(df, aes(concentration, CC_AR)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y~x) +
      stat_regline_equation(label.x = 500, aes(label = ..eq.label..)) +
      stat_regline_equation(aes(label = ..rr.label..)) +
      facet_grid(~Plate, scales = "free_y") +
      ggtitle(df$component_name) +
      xlab("Concentration (fmol)")
    
    p2 <- ggplot(df, aes(concentration, CC_AR)) +
      geom_point() +
      xlim(0, 4) +
      ylim(0, max(df$CC_AR[df$concentration < 4])) +
      facet_grid(~Plate, scales = "free_y") +
      ggtitle("Zoomed") +
      xlab("Concentration (fmol)")
      
    p1 / p2
  })

# pdf("output/HSP_BloodCollection_CalibrationCurve_static.pdf", onefile = T, width = 8, height = 10)
# for (i in seq(length(static))) {
#   print(static[[i]])
# }
# dev.off()

write_csv(regression, "R/Examples/data/5.csv")

## ---

interactive <- data_calib %>% 
  group_split(component_name) %>% 
  # .[4:5] %>%
  lapply(., function(df) {
    p1 <- ggplot(df, aes(concentration, CC_AR)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y~x) +
      facet_grid(~Plate, scales = "free_y") +
      ggtitle(df$component_name) +
      xlab("Concentration (fmol)")
    
    gp <- ggplotly(p1)
  })

# htmltools::save_html(interactive, "output/HSP_BloodCollection_CalibrationCurve_interactive.html")
