# beginning of r script --------------------------------------------------------

# 1. libraries ---------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(lubridate)
library(ggspatial)
library(arcpullr)
library(pdftools)
library(cowplot)
library(tigris)
library(raster)
library(readxl)
library(units)
library(sf)

options(scipen = 999)

# 2. load data ----

## 2.1 census (tracts) ----

### 2.1.1 prep ----

api_key <- "e0e0537e1640689fd0719a4f97581e336b10c8ce"
census_api_key(api_key, overwrite=TRUE, install=TRUE)

readRenviron("~/.Renviron")

options(tigris_use_cache = TRUE)

### 2.1.2 review census tables ----

## use this link for reference to census tables: https://www.socialexplorer.com/data/C2020/metadata/?ds=SF1&table=P0120

## identifying sex by age table

# decennial_tables <- load_variables(2010, "sf1")
# view(decennial_tables)
# P012 = sex by age

### 2.1.3 import census data ----

## downloading population counts for PA block groups in 12 study counties by 
## sex and age from the 2010 decennial census

pop.est.00 <- get_decennial(geography = "tract", # geogaphic units
                   table = "P012",               # list of variables to include
                   year = 2010,                  # final year of data set over 5-yr period
                   state = "PA",                 # restricting to only PA
                   geometry = TRUE,              # attach shapefiles to data set
                   keep_geo_vars = TRUE,     
                   sumfile = "sf1",              # ACS 5-year data set type
                   output = "wide"               # data structure as a wide file
)

## visually reviewing the shapefiles

# ggplot(pop.est.00) +
#   geom_sf()

pop.est.00$area.tract <- abs(st_area(pop.est.00)) # calculating area of each census tract polygon to later determine the proportion of overlap between geographic units to calculate a more accurate population count. use absolute value to prevent negative numbers for polygons within other polygons

## 2.2 pasda (public water supply areas) ----

pws.shp <- get_spatial_layer("https://mapservices.pasda.psu.edu/server/rest/services/pasda/DEP2/MapServer/8", sf_type = "esriGeometryPolygon") %>% # downloading data directly from pasda website
  st_set_crs(st_crs(4269)) %>% # set coordinate system to match other geo files
  st_transform(crs = 4269) # set projection to match other geo files

## repairing simple geometry issues in pws shapefiles
pws.shp <- st_make_valid(pws.shp)

## visually reviewing the shapefiles
# ggplot(pws.shp) +
#   geom_sf()

# calculating area of each census tract polygon to later determine the proportion of overlap between geographic units to calculate a more accurate population count. use absolute value to prevent negative numbers for polygons within other polygons
pws.shp$area.pws <- st_area(pws.shp)

## 2.3 spatial pfas testing data ----

## this step was performed with the support of this blog post: https://medium.com/swlh/the-adventure-of-pdf-to-data-frame-in-r-f90609035600

## url to pa department of environmental protections sampling data of the PDF file
pdf_url <- "https://files.dep.state.pa.us/Water/DrinkingWater/Perfluorinated%20Chemicals/SamplingResults/PFAS_Sampling_Final_Results_May_2021.pdf"

## extract text from the PDF
pdf_text <- pdf_text(pdf_url) %>%
              str_split("\n") # splitting text into rows

## get rid of first 1 to 11 lines
for(i in 1:6) { 
  pdf_text[[i]] <- pdf_text[[i]][-1:-11]
}

## getting rid of definitions at the end of the last page
pdf_text[[6]]  <- pdf_text[[6]][-23:-34]

## removing excess spaces between substrings and splitting rows by common string patterns
pdf_text <- pdf_text %>%
              str_squish() %>%
              strsplit(split= "\\,\\s\\\"")

## removing common substring at the start of each page
for(i in 1:length(pdf_text)) {
  pdf_text[[i]][1] <- pdf_text[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*")
}

## removing common substring at the end of each line
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text[[i]])) {
    pdf_text[[i]][j] <- pdf_text[[i]][j] %>%
      stringr::str_extract(".*(?=\")")
  }
}

## separating characters
for(i in 1:length(pdf_text)) {
  for(j in 1:length(pdf_text)){
    pdf_text[[i]][j] %>% str_extract(".*[:alpha:]+|\\&|\\-") %>% 
      print() #extracts the words
  }
}

## put into a single column of a data frame
names_ex = list()
for(i in 1:length(pdf_text)) {
  words <- pdf_text[[i]] %>% str_extract(".*[:alpha:]+|\\&|\\-") 
  words_df <- data.frame(words) #turns into data frame for list
  names_ex[[i]] <- words_df
  NH_names <- dplyr::bind_rows(names_ex) %>% drop_na() # removing black row at the end of each page
}
# print(NH_names)

for(i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][nrow(str_locate_all(NH_names$words, " ")[[i]])-20]
}

right <- str_sub(NH_names$words, start = unlist(NH_names$loc))
ds <- cbind(NH_names,right)

for(i in 1:nrow(NH_names)) {
  NH_names$loc[[i]] <- str_locate_all(NH_names$words, " ")[[i]][5]
}

left <- str_sub(NH_names$words, end = unlist(NH_names$loc))
ds <- cbind(ds,left)

pfas.ds <- dplyr::select(cbind(tidyr::separate(ds, col = left, into = c("drop00","drop01","pwsid","drop02","drop03"), sep=" "), 
                              tidyr::separate(ds, col = right, into = c("drop04","date","pfas01","pfas02","pfas03","pfas04","pfas05","pfas06","pfas07","pfas08","pfas09","pfas10","pfas11","pfas12","pfas13","pfas14","pfas15","pfas16","pfas17","pfas18","pfas19","drop05"), sep=" ")),
          -words, -loc, -left, -right, -starts_with("drop")
          ) %>%
          mutate(positive.pfas = ifelse(if_any(starts_with("pfas"), ~.x == "0.0"), 0, 1)) %>%
          dplyr::select(-starts_with("pfas")) %>%
          group_by(pwsid) %>%
          summarize(positive.pfas = max(positive.pfas))

# merging together

pws.pfas <- st_as_sf(merge(pfas.ds, pws.shp, by.x = "pwsid", by.y = "PWS_ID", all = FALSE))

## visually reviewing the shapefiles

# ggplot(pws.pfas) +
#   geom_sf()

# 3. prepping data for processing/analysis ----

## 3.1 calculating pop counts ----

pop.est.01 <- pop.est.00 %>% 
                mutate(
                  pop.total = P012001, 
                  pop.youth = P012003 + P012004 + P012005 + P012006 + P012027 + P012028 + P012029 + P012030,
                  pop.adult = pop.total - pop.youth
                ) %>%
                dplyr::select(GEOID, area.tract, COUNTY, starts_with("pop")) # only retaining necessary variables 

## 3.2 subsetting pws with testing data (exposure) ----

pws.shp.pfas <- pws.pfas %>% # merge pws shapefiles and pfas testing data, retaining merge matches on x and y
  filter(positive.pfas == 1 & !is.na(area.pws) & !(OWNERSHIP %in% c("Mobile Home Park","Apartments","Institutional Recreational","Institutional Military","Institutional Health","Institutional Education","Institutional Correctional"))) %>% # retaining pws with testing data and non-missing and not an apartment, mobile home park, institutional corrections
  dplyr::select(pwsid, WUDS_ID, CNTY_NAME, area.pws, positive.pfas) # subsetting to only necessary variables


# 4. census tracts ----

## 4.1 determining overlap between census tracts and pws ----

## visualizing overlap before processing to see what should be expected 
# ggplot() +
#   geom_sf(data = pws.shp.pfas, aes(color="red", fill = "red")) + # all polygons will be solid red
#   geom_sf(data = pop.est.01, alpha = 0) + # will be a black outline of each polygon
#   theme_void() +
#   theme(legend.position = "none")

## performing intersection to output a new shapefile with all overlapping polygons
tract.pws.int <- unique(st_intersection(pws.shp.pfas, pop.est.01))

## visualizing the results of the intersection with pws
# ggplot() +
#   geom_sf(data = pws.shp.pfas, aes(color="red", fill = "red")) + # all polygons will be solid red
#   geom_sf(data = pop.est.01, alpha = 0, color = "white") + # will be a white outline of each polygon
#   geom_sf(data = tract.pws.int, alpha = 0) + # will be a black outline of each polygon
#   theme_void() +
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "grey"))

## calculating area of intersection
tract.pws.int$area.int <- st_area(tract.pws.int)
  
## calculating the % of tract area overlapping a pws 
tract.pws.int$int.pct <- tract.pws.int$area.int/tract.pws.int$area.tract

## 4.2 calculate exposed population ----

## multiply the population for each tract by the proportion of the area that
## overlaps with a pws then sum by tract to calculate the new weighted population 
## which represents the relative population for each tract that are exposed to 
## public drinking water tested for pfas

tract.pws.int2 <- drop_units(tract.pws.int) %>%
  group_by(GEOID) %>%
  summarize(
    
    # weighted population
    total.wtd = sum(pop.total*int.pct), 
    adult.wtd = sum(pop.youth*int.pct), 
    youth.wtd = sum(pop.adult*int.pct),

    # creating the aggregated raw variables to calculate the total exposed eligible sample
    pop.total = sum(pop.total), 
    pop.youth = sum(pop.youth), 
    pop.adult = sum(pop.adult),
    across()
  )

## total pop from tracts that overlap with exposure data 
study.area.pop <- round(sum(tract.pws.int2$total.wtd),0)

## calculating the pct of the population that make up the weighted proportions
tract.pws.int3 <- tract.pws.int2 %>%
  mutate(
    total.wtd_pct = (total.wtd/study.area.pop)*100, 
    adult.wtd_pct = (adult.wtd/study.area.pop)*100, 
    youth.wtd_pct = (youth.wtd/study.area.pop)*100
  )

## merging intersected polygons back with the original census tract polygons
tract.merge <- merge(
  dplyr::select(pop.est.00, GEOID), # only retaining necessary variables
  st_drop_geometry(tract.pws.int3),
  by = "GEOID", keep.y = TRUE) # only keeping tracts that intersected with a pws

## checking that the full tract polygon is used rather than only the intersection
# ggplot(tract.merge) +
#   geom_sf()

## visually reviewing the results
# ggplot(tract.merge) +
#   geom_sf(aes(fill = total.wtd_pct, color = total.wtd_pct))

# 5. figure ----

## 5.1 creating county object for map borders ----

pa.counties <- counties(state = "42", cb = TRUE, class = "sf")

pa.border <- filter(states( cb = TRUE, class = "sf"), GEOID == "42")

## 5.2 calculating quintiles for total pct by geo type ----

## total
tract.merge$total.wtd_pct.q <- cut(tract.merge$total.wtd_pct, quantile(tract.merge$total.wtd_pct, probs=c(0,.2,.4,.6,.8,1)), label=FALSE, include.lowest=TRUE) 

## adult
tract.merge$adult.wtd_pct.q <- cut(tract.merge$adult.wtd_pct, quantile(tract.merge$adult.wtd_pct, probs=c(0,.2,.4,.6,.8,1)), label=FALSE, include.lowest=TRUE) 

## youth
tract.merge$youth.wtd_pct.q <- cut(tract.merge$youth.wtd_pct, quantile(tract.merge$youth.wtd_pct, probs=c(0,.2,.4,.6,.8,1)), label=FALSE, include.lowest=TRUE) 

## 5.3 maps ----

### 5.3.1 all pws w pfas ----

## set color scheme
col <-  c("grey","#08306b")

ggplot(pws.pfas) +
  geom_sf(color = NA,  aes(fill = as.factor(positive.pfas))) + # removes boundary lines around polygons
  coord_sf(datum=NA, expand=FALSE) + # sets projection for map
  geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
  scale_fill_manual(values = col, labels = c("No PFAS",'PFAS Detected'), limits= c("0","1"), drop=FALSE, na.value = "white") +
  scale_color_manual(values = col, labels = c("No PFAS",'PFAS Detected'), drop=FALSE, na.value = "white") +
  labs(caption="Areas with PFAS+ Public Drinking Water in Pennsylvania") +   # subtitle... Census 2010 100% population counts
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(color = "black", size = 3),
    title = element_text(family="Arial"),
    plot.caption.position = "panel",
    plot.caption = element_text(size=12,hjust=0.5),
    legend.text = element_text(family="Arial", hjust = 0.5, size = 10),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "bottom"
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )) 

ggsave("02_output/pa_pfas.png", height=5.1, width=7, units="in")

map.pws <- ggplot(pws.pfas) +
            geom_sf(color = NA,  aes(fill = as.factor(positive.pfas))) + # removes boundary lines around polygons
            coord_sf(datum=NA, expand=FALSE) + # sets projection for map
            geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
            scale_fill_manual(values = col, labels = c("No PFAS",'PFAS Detected'), limits= c("0","1"), drop=FALSE, na.value = "white") +
            scale_color_manual(values = col, labels = c("No PFAS",'PFAS Detected'), drop=FALSE, na.value = "white") +
            theme_void()

### 5.3.2 adult pop ----

## set color scheme
cols.blues <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08306b")

ggplot(tract.merge) +
  geom_sf(color = NA,  aes(fill = as.factor(adult.wtd_pct.q))) + # removes boundary lines around polygons
  coord_sf(datum=NA, expand=FALSE) + # sets projection for map
  geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
  scale_fill_manual(values = cols.blues, labels = c('[0% < 20%]','[20% < 40%]','[40% < 60%]','[60% < 80%]','[80% - 100%]'), limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  scale_color_manual(values = cols.blues, limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  guides(fill=guide_legend(title="Population Density Quintiles", title.position = "bottom", title.hjust = 0.5), color="none") +
  labs(caption="Adult Pennsylvanians Exposed to PFAS Drinking Water at the Census Tract Level") +   # subtitle... Census 2010 100% population counts
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(color = "black", size = 3),
    title = element_text(family="Arial"),
    plot.caption.position = "panel",
    plot.caption = element_text(size=12,hjust=0.5),
    legend.text = element_text(family="Arial", hjust = 0.5, size = 10),
    legend.title = element_text(family="Arial", size = 12),
    legend.key.size = unit(0.5, "cm"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "bottom"
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )) 

ggsave("02_output/ct_exposed_adultpop.png", height=5.1, width=7, units="in")

map.adult <- ggplot(tract.merge) +
  geom_sf(color = NA,  aes(fill = as.factor(adult.wtd_pct.q))) + # removes boundary lines around polygons
  coord_sf(datum=NA, expand=FALSE) + # sets projection for map
  geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
  scale_fill_manual(values = cols.blues, labels = c('[0% < 20%]','[20% < 40%]','[40% < 60%]','[60% < 80%]','[80% - 100%]'), limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  scale_color_manual(values = cols.blues, limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  theme_void() 

### 5.3.3 youth pop ----

ggplot(tract.merge) +
  geom_sf(color = NA,  aes(fill = as.factor(youth.wtd_pct.q))) + # removes boundary lines around polygons
  coord_sf(datum=NA, expand=FALSE) + # sets projection for map
  geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
  scale_fill_manual(values = cols.blues, labels = c('[0% < 20%]','[20% < 40%]','[40% < 60%]','[60% < 80%]','[80% - 100%]'), limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  scale_color_manual(values = cols.blues, limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  guides(fill=guide_legend(title="Population Density Quintiles", title.position = "bottom", title.hjust = 0.5), color="none") +
  labs(caption="Adolescent Pennsylvanians Exposed to PFAS Drinking Water at the Census Tract Level") +   # subtitle... Census 2010 100% population counts
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(color = "black", size = 3),
    title = element_text(family="Arial"),
    plot.caption.position = "panel",
    plot.caption = element_text(size=12,hjust=0.5),
    legend.text = element_text(family="Arial", hjust = 0.5, size = 10),
    legend.title = element_text(family="Arial", size = 12),
    legend.key.size = unit(0.5, "cm"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "bottom"
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0, "in"),
    height = unit(0.6, "in"), width = unit(0.6, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )) 

ggsave("02_output/ct_exposed_youthpop.png", height=5.1, width=7, units="in")

map.youth <- ggplot(tract.merge) +
  geom_sf(color = NA,  aes(fill = as.factor(adult.wtd_pct.q))) + # removes boundary lines around polygons
  coord_sf(datum=NA, expand=FALSE) + # sets projection for map
  geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
  scale_fill_manual(values = cols.blues, labels = c('[0% < 20%]','[20% < 40%]','[40% < 60%]','[60% < 80%]','[80% - 100%]'), limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  scale_color_manual(values = cols.blues, limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
  theme_void() 

### 5.3.4 counties w largest exposed pop ----

cnty <- dplyr::select(as.data.frame(st_drop_geometry(pa.counties)), COUNTYFP, NAME)

tract.merge <- merge(x = tract.merge, y = cnty, by.x = "COUNTY", by.y = "COUNTYFP", all.x = TRUE)

view(
  tract.merge %>%
    group_by(NAME) %>%
    summarise(total.pop.sum = sum(total.wtd)) %>%
    arrange(-total.pop.sum) %>%
    dplyr::select(NAME, total.pop.sum)
)

gg.cntys <- plot_grid(
              ggdraw() + draw_label("TOP 3 COUNTIES BY EXPOSED POPULATION", fontfamily = "Arial", fontface = "bold", size = 12),
              ggdraw() + draw_label("Montgomery, Delaware, Chester: 1,323,472", fontfamily = "Arial", fontface = "bold", size = 12),
              ggdraw() + draw_label("62.3% of exposed population", fontfamily = "Arial", fontface = "bold", size = 12),
              ggplot(filter(pws.pfas, CNTY_NAME %in% c("Montgomery","Delaware","Chester"))) +
                geom_sf(color = NA,  aes(fill = as.factor(positive.pfas))) + # removes boundary lines around polygons
                coord_sf(datum=NA, expand=FALSE) + # sets projection for map
                geom_sf(data = filter(pa.counties, NAME %in% c("Montgomery","Delaware","Chester")), fill = NA, color = "black", size = 1) +
                scale_fill_manual(values = "#08306b", labels = c('PFAS Detected'), limits= c("1"), drop=FALSE, na.value = "white") +
                scale_color_manual(values = "#08306b", labels = c('PFAS Detected'), drop=FALSE, na.value = "white") +
                theme_void() +
                theme(legend.position = "none"),
              ncol = 1,
              rel_heights = c(.5,.5,.5,10)
            ) + theme(plot.background = element_rect(fill = "white"), plot.margin = margin(0.5,0,0,0, "cm"))

ggsave("02_output/top3_cntys.png", height=4, width=4, units="in")

### 5.3.5 bar chart w population #s in the bars and % at the end ----

## total population for PA = 12,702,379
pop.total <- sum(pop.est.00$P012001)

## total population exposed to PFAS = 2,124,371
pop.total.pfas <- sum(tract.merge$total.wtd)

## adult population exposed to PFAS = 483,483
pop.adult.pfas <- sum(tract.merge$adult.wtd)

## adult population exposed to PFAS = 1,640,888
pop.youth.pfas <- sum(tract.merge$youth.wtd)

# pop.total.pfas
# pop.adult.pfas
# pop.youth.pfas
# 
# (pop.total.pfas/pop.total)*100
# (pop.adult.pfas/pop.total)*100
# (pop.youth.pfas/pop.total)*100

bar.ds <- data.frame("pop" = c("pa", "pfas", "pfas.youth", "pfas.adult"), 
                     "pct" = c(100, 16.72, 12.91, 3.80)
)

bar.chart <- ggplot(bar.ds, aes(pop, pct)) +
              geom_col(fill = "#08306b", color = "#08306b") + 
              scale_y_continuous(limits = c(0,100), expand = c(0,0), breaks=seq(0,100, by = 50)) +
              scale_x_discrete(limits = c("pfas.adult", "pfas.youth", "pfas", "pa")) +
              coord_flip(clip = "off") +
              theme_void() + 
              theme(plot.margin = margin(1,1,1,0.5, "cm"),
                    panel.spacing = margin(1,0,0,0, "cm"),
                    plot.background = element_rect(fill = "white"),
                    axis.line.x = element_line(color = "black", size = 1),
                    axis.ticks.x = element_line(color = "black", size = 1),
                    axis.ticks.length.x = unit(.5, "cm"),
                    axis.text.x = element_text(color = "black", size = 16, family = "Arial", vjust = -1.5)) +
              annotate("text", label = "PENNSYLVANIA POPULATION", x = 5, y = 51.5, vjust = 1, color = "black", fontface = "bold", family = "Arial", size = 7) +   
              annotate("text", label = "12,702,379", x = 4.02, y = 50, color = "white", fontface = "bold", family = "Arial", size = 12) + 
              annotate("text", label = "EXPOSED TO PFAS: 2,124,371", x = 3.02, y = 34, hjust = 0.2, color = "#08306b", fontface = "bold", family = "Arial", size = 5) +
              annotate("text", label = "EXPOSED YOUTH: 1,640,888", x = 2.02, y = 43, hjust = 0.4, color = "#08306b", fontface = "bold", family = "Arial", size = 5) +
              annotate("text", label = "EXPOSED ADULTS: 483,483", x = 1.02, y = 40, color = "#08306b", fontface = "bold", family = "Arial", size = 5)

# ggsave("02_output/pop_barchart.png", height=7, width=7, units="in")

### 5.3.6 static dashboard ----

map.pws <- ggplot(pws.pfas) +
            geom_sf(color = NA,  aes(fill = as.factor(positive.pfas))) + # removes boundary lines around polygons
            coord_sf(datum=NA, expand=FALSE) + # sets projection for map
            geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
            scale_fill_manual(values = col, labels = c("No PFAS",'PFAS Detected'), limits= c("0","1"), drop=FALSE, na.value = "white") +
            scale_color_manual(values = col, labels = c("No PFAS",'PFAS Detected'), drop=FALSE, na.value = "white") +
            theme(
              panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              title = element_text(family="Arial"),
              plot.caption.position = "panel",
              plot.caption = element_text(size=12,hjust=0.5),
              legend.text = element_text(family="Arial", hjust = 0.5, size = 10),
              legend.title = element_blank(),
              legend.key.size = unit(0.5, "cm"),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position = "bottom",
              plot.margin = margin(0,0,0,0, "cm"),
              panel.spacing = margin(0,0,0,0, "cm"),
              legend.margin = margin(0,0,0,0, "cm"),
              legend.box.margin = margin(-1,0,0,0, "cm")
            )

map.total <- ggplot(tract.merge) +
              geom_sf(color = NA,  aes(fill = as.factor(total.wtd_pct.q))) + # removes boundary lines around polygons
              coord_sf(datum=NA, expand=FALSE) + # sets projection for map
              geom_sf(data = pa.counties, fill = NA, color = "grey25", size = 0.8) +
              scale_fill_manual(values = cols.blues, labels = c('[0% < 20%]','[20% < 40%]','[40% < 60%]','[60% < 80%]','[80% < 100%]'), limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
              scale_color_manual(values = cols.blues, limits= c("1","2","3","4","5"), drop=FALSE, na.value = "white") +
              guides(fill=guide_legend(title="Population Density Quintiles", title.position = "bottom", title.hjust = 0.5), color="none") +
              theme(
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white"),
                title = element_text(family="Arial"),
                plot.caption.position = "panel",
                plot.caption = element_text(size=12,hjust=0.5),
                legend.text = element_text(family="Arial", hjust = 0.5, size = 10),
                legend.title = element_text(family="Arial", hjust = 0.5, size = 10),
                legend.key.size = unit(0.5, "cm"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.position = "bottom",
                plot.margin = margin(0,0,0,0, "cm"),
                panel.spacing = margin(0,0,0,0, "cm"),
                legend.margin = margin(0,0,0,0, "cm"),
                legend.box.margin = margin(-0.6,0,0.1,0, "cm")
              )

maps <- plot_grid(
          map.pws + theme(plot.caption = element_blank(), plot.margin = margin(0,0,0,0, "cm")),
          map.total + theme(plot.caption = element_blank(), plot.margin = margin(0,0,0,0, "cm")),
          ncol = 1,
          rel_heights = c(0.95,1)
        ) + theme(plot.background = element_rect(fill = "white"), plot.margin = margin(0,0,0,0, "cm"))

# ggsave("02_output/map_combined.png", height=6, width=4, units="in")

plot_grid(
    ggdraw() + draw_label("Distribution of PFAS-contaminated Public Water Supply in Pennsylvania", fontfamily = "Arial", fontface = "bold", size = 22),
    plot_grid(
      maps + theme(plot.background = element_rect(color = "white", fill = "white")),
      plot_grid(
        gg.cntys + theme(plot.background = element_rect(color = "white", fill = "white"), plot.margin = margin(0,0,0,0, "cm")),
        bar.chart + theme(plot.background = element_rect(color = "white", fill = "white")),
        ncol = 1
      ),
    rel_widths = c(1.5,1),
    ncol = 2
    ),
    nrow = 2,
    rel_heights = c(1,10)
) + theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("02_output/pfas_dashboard.png", height=8.5, width=11, units="in")


# end of r script --------------------------------------------------------------


