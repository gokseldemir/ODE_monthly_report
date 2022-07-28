##################################################
###########SURVEILLANCE MAPPING CODE##################
############################################
##Created by: Goksel
##Created on: May 12, 2021
##Purpose: to develop LHA/HSDA based maps
##Modified by:
##Modified reasons:
#########################################################

library(tidyverse)
library(sf)
library(bcmaps)
library(tmap)

fn_events <- bcehs2 %>% 
  filter(FN_FLAG == 1) %>%
  filter(DATE_SERVICE <= ymd("2021-10-31")) %>%
  dplyr::group_by(year, DERIVED_LHA) %>% 
  dplyr::summarise(total_ods = n()) %>%  
  pivot_wider(names_from = year, 
              values_from = total_ods, 
            #  values_fill = list(total_ods=0)
              ) %>% 
  mutate(LOCAL_HLTH_AREA_CODE=as.character(DERIVED_LHA))
  

fn_lha_pop <- read_csv(file = "//fnha.local/groups/Health Surveillance/Health Surveillance - General/Population files/FNCF Population/2018 FNCF/FNCF_2018_population_by_LHA.csv")

fn_lha_pop <- fn_lha_pop %>% select(-X1) %>% 
  mutate(LOCAL_HLTH_AREA_CODE=as.character(lha_id_2018))


od_shape <- health_lha() %>% 
  left_join(fn_events, by="LOCAL_HLTH_AREA_CODE") %>% 
  left_join(fn_lha_pop, by="LOCAL_HLTH_AREA_CODE")
  
#quantiles - you want the equal number of data points in each categories
#range of values broken up into equal categoris
tm_shape(od_shape) + 
  tm_polygons("2020", 
              style = "pretty") 
 # tm_text("LOCAL_HLTH_AREA_NAME")

vch <- od_shape %>% 
  filter(HLTH_AUTHORITY_CODE == "3")

vancouver <- od_shape %>% 
  filter(HLTH_SERVICE_DLVR_AREA_CODE == "32")


tm_shape(vch) + 
  tm_polygons("2021")+
  tm_text("LOCAL_HLTH_AREA_NAME")

##############################################
####HSDA events 
fn_hsda_events <- bcehs2 %>% 
  filter(FN_FLAG == 1) %>% 
  dplyr::group_by(year, DERIVED_HSDA) %>% 
  dplyr::summarise(total_ods = n()) %>%  
  pivot_wider(names_from = year, 
              values_from = total_ods) %>% 
  mutate(HLTH_SERVICE_DLVR_AREA_CODE=as.character(DERIVED_HSDA))

od_hsda_shape <- health_hsda() %>% 
  left_join(fn_hsda_events, by="HLTH_SERVICE_DLVR_AREA_CODE")

fraser <- od_hsda_shape %>% 
  filter(HLTH_AUTHORITY_CODE == "2")

tm_shape(fraser) + 
  tm_polygons("2020", 
              style = "fixed", 
              breaks = c(100, 125, 150, 175, 200))+
  tm_text("HLTH_SERVICE_DLVR_AREA_NAME") + 
  tm_text("2020")

tm_shape(od_hsda_shape) + 
  tm_polygons("2020", 
              style = "pretty",
              title = "Number of drug poisoning events, 2020")+
  tm_text("HLTH_SERVICE_DLVR_AREA_NAME") +
  tm_layout(legend.text.size = 1)


tm_shape(fraser) + 
  tm_polygons("2020", 
              style = "fixed", 
              breaks = c(1, 101, 201, 301, 401, 501, 601, 700))+
  tm_text("HLTH_SERVICE_DLVR_AREA_NAME") + 
  tm_text("2020")


#######LHA based map, 
thecols <- c("2018", "2019",  "2020", "2021"  )
od_shape_suppressed <- od_shape  %>% 
  mutate_at(thecols, ~ifelse(.x<11, NA, .x))

od_shape_nona <- od_shape %>% 
  mutate_at(thecols, ~ifelse(is.na(.x), 0, .x))


####With predetermined breaks
tm_shape(od_shape) + 
  tm_polygons("2020", 
              style = "quantile", 
            #  breaks = c(1, 51, 101, 151, 201, 251, 500),
              title = "Number of drug poisoning events, 2020", 
             # labels = c("1 to 50", "51 to 100", "101 to 150", "151 to 200", "201 to 250", ">250"), 
              textNA = "No events reported") + 
  tm_layout(legend.text.size = 1, 
            legend.position = c("left", "bottom")) 

fraser_lha <- od_shape %>% 
  filter(HLTH_SERVICE_DLVR_AREA_CODE == "32")

tm_shape(fraser_lha) + 
  tm_polygons("2020", 
              style = "fixed", 
              breaks = c(1, 51, 101, 151, 201, 251, 500),
              title = "Number of drug poisoning events, 2020", 
              labels = c("1 to 50", "51 to 100", "101 to 150", "151 to 200", "201 to 250", ">250"), 
              textNA = "No events reported") + 
  tm_text("LOCAL_HLTH_AREA_NAME") + 
  tm_layout(legend.text.size = 1) 
 
###suppression map
tm_shape(od_shape_suppressed) + 
  tm_polygons("2020", 
              style = "fixed", 
              breaks = c(11, 51, 101, 151, 201, 251, 500),
              title = "Number of drug poisoning events, 2020", 
             # labels = c("1 to 50", "51 to 100", "101 to 150", "151 to 200", "201 to 250", ">250"), 
              textNA = "Suppressed"
             ) + 
  tm_layout(legend.text.size = 1, 
            legend.position = c("left", "bottom")) 


##################################
fn_lha_pop_b <- fn_lha_pop %>% filter(year == 2019)

fn_rates <- fn_events %>% 
  mutate(lha_id_2018=as.integer(DERIVED_LHA)) %>%
# rename(lha_id_2018 = DERIVED_LHA) %>% 
  left_join(fn_lha_pop_b, by = "lha_id_2018") %>% 
  mutate(rate = round(1000*`2020`/population, digits=1), 
         supp_rate_20_per_1000 = ifelse(is.na(`2020`), 0, 
                                        ifelse(`2020` <11, NA, 
                                        rate)))
############

 
         
ggplot(fn_rates, aes(rate)) + 
  geom_histogram()
############################################3
od_shape_rates <- od_shape %>% 
  mutate(od_rate_20_per_1000 = round(1000*`2020`/population, digits=1), 
         supp_rate_20_per_1000 = ifelse(is.na(`2020`), 0, 
                                        ifelse(`2020` <11, NA, 
                                               od_rate_20_per_1000)))

tm_shape(od_shape_rates) + 
  tm_polygons("supp_rate_20_per_1000", 
              style = "fixed", 
              breaks = c(0, 10.1, 20.1, 30.1, 40.1, 200),
             # labels = c("<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", ">40.0"),
              textNA = "Suppressed(n<11)", 
              title= "Rate per 1,000, 2020") +
  tm_layout(legend.text.size = 0.57, 
            legend.outside=T) 

# ggsave("lha_rate_maps_BC.jpeg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2021/September",  width =  12.85, height = 7.8)
# ggsave("lha_rate_maps_BC.jpeg", width=9, height=7)

vch_fraser <- od_shape_rates %>% filter(HLTH_SERVICE_DLVR_AREA_CODE %in% c(32, 31))

tm_shape(vch_fraser) + 
  tm_polygons("od_rate_20_per_1000", 
              style = "fixed", 
              breaks = c(0, 10.1, 20.1, 30.1, 40.1, 100),
              labels = c("<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", ">40.0"),
              textNA = "No events reported", 
              title = "Rate per 1,000, 2020") + 
  tm_text("LOCAL_HLTH_AREA_NAME", remove.overlap=T) + 
  tm_layout(legend.text.size = 1, 
            #legend.position = c("right", "top"),
            legend.outside=T)

tmap_save(filename = "lha_rate_maps.jpeg", width=9, height=7)

####################################################
###One yr rolling events 
oneyrroll <- seq(max(bcehs2$dateofevent_floor), by="-1 month", length=12)

fn_events <- bcehs2 %>% 
  filter(FN_FLAG == 1 & dateofevent_floor%in% oneyrroll) %>% 
  dplyr::group_by(DERIVED_LHA) %>% 
  dplyr::summarise(total_ods = n()) %>%  
  mutate(LOCAL_HLTH_AREA_CODE=as.character(DERIVED_LHA))

fn_hsda_events2 <- bcehs2 %>% 
  filter(FN_FLAG == 1 & dateofevent_floor%in% oneyrroll) %>% 
  dplyr::group_by(DERIVED_HSDA) %>% 
  dplyr::summarise(total_ods = n()) %>% 
  mutate(HLTH_SERVICE_DLVR_AREA_CODE=as.character(DERIVED_HSDA))

####################################################
### One year rolling map HSDA

#Creating Population File
fn_hsda_pop <- read_csv(file = "//fnha.local/groups/Health Surveillance/Health Surveillance - General/Population files/FNCF Population/2018 FNCF/FNCF_2018_population_by_LHA.csv")
# fn_hsda_pop <- as.data.frame(fn_hsda_pop)

#Aggregating LHAs population to obtain HSDA population
fn_hsda_pop <- aggregate(population ~ hSda_id, fn_hsda_pop, sum)

#Filtering and Preparing HSDA Population File
fn_hsda_pop <- fn_hsda_pop %>%
  mutate(HLTH_SERVICE_DLVR_AREA_CODE=as.character(hSda_id))

#Linking HSDA Population File and deaths and events all together 
fn_hsda_events3 <-  fn_hsda_pop %>%
  left_join(fn_hsda_events2, by="HLTH_SERVICE_DLVR_AREA_CODE") 

#Suppressing events (n <= 10) 
fn_hsda_events3 <- fn_hsda_events3 %>% 
  mutate(`total_ods` = ifelse(`total_ods` <=10, NA, `total_ods`)) 

#Calculating death ratio, event ratio, deaths_to_event ratio Grouped by HSDA!!
fn_hsda_events3 <- fn_hsda_events3 %>%
  mutate(rate_per_1000_roll = round(1000*total_ods/population, digits=1))


od_hsda_shape_roll_rates <- health_hsda() %>% 
  left_join(fn_hsda_events3, by="HLTH_SERVICE_DLVR_AREA_CODE") %>%
  mutate(supp_rate_roll_per_1000 = ifelse(is.na(total_ods), 0, 
                                          ifelse(total_ods <11, NA, 
                                                 rate_per_1000_roll)), 
         rank = ifelse(is.na(supp_rate_roll_per_1000), NA, 
                       rank(desc(supp_rate_roll_per_1000))), 
         map_label = ifelse(rank<7, HLTH_SERVICE_DLVR_AREA_CODE, NA))

HSDA_per1000 <- tm_shape(od_hsda_shape_roll_rates) + 
  tm_polygons("supp_rate_roll_per_1000", 
              style = "fixed", 
              breaks = c(0, 1, 2, 3, 4, 6, 20),
              labels = c("<1", "1-2", "2-3", "3-4", "4-10", ">=10"),
              textNA = "Suppressed(n<11)", 
              title= "Rate per 1,000, Past 1 year rolling") +
  tm_layout(legend.text.size = 0.57, 
            legend.outside=T, 
            main.title =  "Drug Poisoning Event Rates per 1,000, Rolling 1 year", 
            title = "November 2020 - October 2021") #+ 
# tm_text("map_label",  remove.overlap = T, shadow = T, overwrite.lines = T, auto.placement = T, size=0.6)

# tmap_save(filename = here("September", "bc_oct20_sep21.jpeg"), width=9, height=7)
tmap_save(tm = HSDA_per1000, filename = "bc_apr21_mar22_HSDA.jpeg")

####################################################
### One year rolling map LHA

fn_lha_pop_b <- fn_lha_pop_b %>% select(-LOCAL_HLTH_AREA_CODE)
fn_rates_roll <-  fn_events %>% 
  mutate(lha_id_2018=as.integer(DERIVED_LHA)) %>%
  # rename(lha_id_2018 = DERIVED_LHA) %>% 
    left_join(fn_lha_pop_b, by = "lha_id_2018") %>% 
  mutate(rate_per_1000_roll = round(1000*total_ods/population, digits=1))

ggplot(fn_rates_roll, aes(rate_per_1000_roll)) + 
  geom_histogram()

od_shape_roll_rates <- health_lha() %>% 
  left_join(fn_rates_roll, by="LOCAL_HLTH_AREA_CODE") %>% 
  mutate(supp_rate_roll_per_1000 = ifelse(is.na(total_ods), 0, 
                                        ifelse(total_ods <11, NA, 
                                               rate_per_1000_roll)), 
         rank = ifelse(is.na(supp_rate_roll_per_1000), NA, 
                       rank(desc(supp_rate_roll_per_1000))), 
         map_label = ifelse(rank<7, LOCAL_HLTH_AREA_NAME, NA))

LHA_per1000 <- tm_shape(od_shape_roll_rates) + 
  tm_polygons("supp_rate_roll_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1, 200),
              labels = c("No events Reported", "<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.1-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title= "Rate per 1,000, Past 1 year rolling") +
  tm_layout(legend.text.size = 0.57, 
            legend.outside=T, 
            main.title =  "Drug Poisoning Event Rates per 1,000, Rolling 1 year", 
            title = "November 2020 - October 2021") #+ 
 # tm_text("map_label",  remove.overlap = T, shadow = T, overwrite.lines = T, auto.placement = T, size=0.6)

# tmap_save(filename = here("September", "bc_oct20_sep21.jpeg"), width=9, height=7)
tmap_save(tm = LHA_per1000, filename = "bc_apr21_mar22.jpeg")

vch_fraser <- od_shape_roll_rates %>% 
  filter(HLTH_SERVICE_DLVR_AREA_CODE %in% c(32, 31, 23) | lha_id_2018 %in% c(212, 213))

tm_shape(vch_fraser) + 
  tm_polygons("supp_rate_roll_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1, 200),
              labels = c("No events reported","<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.1-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title = "Rate per 1,000") + 
  tm_text("LOCAL_HLTH_AREA_NAME", size = "AREA", root = 5, remove.overlap=F) + 
  tm_layout(legend.text.size = 1, 
            #legend.position = c("right", "top"),
            legend.outside=T)

# tmap_save(filename = here("July", "vchfr_jul20_jun21.jpeg"), width=6, height=2)
tmap_save(filename = "vchfr_apr21_mar21.jpeg")



fraser_map <- od_shape_roll_rates %>% filter(HLTH_AUTHORITY_CODE %in% c(2))

tm_shape(fraser_map) + 
  tm_polygons("supp_rate_roll_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1,  200),
              labels = c("No events Reported", "<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.1-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title = "Rate per 1,000, Past 1 year rolling") + 
  tm_text("LOCAL_HLTH_AREA_NAME") + 
  tm_layout(legend.text.size = 1, 
            #legend.position = c("right", "top"),
            legend.outside=T, 
            main.title = "Drug Poisoning Event Rates per 1,000, First Nations, Fraser, 
            July 1, 2020-June 30, 2021")
tmap_save(filename = "fraser_apr21_mar22.jpeg")

