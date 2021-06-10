library(tidyverse)
library(urbnmapr)
library(sf)
library(viridis)
library(RColorBrewer)


# call in functions
#source('src/theme_ev.R')
#theme_set(theme_ev()) # set ggplot theme
# theme_set(theme_minimal()) # set ggplot theme

# read spatial data with counties and states 
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
states_sf <- get_urbn_map(map = "states", sf = TRUE)

#mob = read.csv('C:\\Users\\noibar\\Downloads\\all_minor_sources\\all_minor_sources.csv')
usa = read_csv('data/mobility_subset.zip', col_types = cols(apple_mobility = col_double(), 
                                                            fips=col_character())) # for some reason this was reading Apple weird -> specify

# add leading zeroes 
add_lead_zero = function(x) {
  ifelse(nchar(x)==4, paste0('0', x), x)
}

usa$fips = add_lead_zero(usa$fips)

glimpse(usa)
#usa = read.csv('data/mobility_subset.csv', colClasses = c("fips"="character"))
# convert to datetime 
#usa$ds_m4 = as.Date(usa$ds_m4)



# create subset of mobility variables 
mob_vars = c("all_day_ratio_single_tile_users", "m50", "apple_mobility", "miles_per_person_umd", "staying_home_umd", "sg_avg_dist2", "sg_sheltered")

mv_names = c("Facebook#Tile Ratio", "Descartes Labs#Median Distance", "Apple#Mobility", "U of Maryland#Miles per Person", 
             "U of Maryland#Pct Staying Home", "SafeGraph#Average Distance", "SafeGraph#Pct Sheltered")

mv_names_ord = c("U of Maryland#Pct Staying Home", "SafeGraph#Pct Sheltered", "SafeGraph#Average Distance", "U of Maryland#Miles per Person", 
                 "Descartes Labs#Median Distance", "Apple#Mobility", "Facebook#Tile Ratio")

# helpful for prettier labels 
var_names <- c(
  "all_day_ratio_single_tile_users" = "Facebook Tile Ratio", 
  "m50" = "Descartes Labs Median Distance", 
  "apple_mobility" = "Apple Mobility", 
  "miles_per_person_umd" = "U of Maryland Miles per Person", 
  "staying_home_umd" = "U of Maryland Pct Staying Home", 
  "sg_avg_dist2" = "SafeGraph Average Distance", 
  "sg_sheltered" = "SafeGraph Pct Sheltered"
)


# simple tally of missing values per variable 
ss = sapply(usa[,mob_vars], function(x) round(sum(is.na(x))/dim(usa)[1],4)*100)

# function helping wrap the labels
addline_format <- function(x,...){
  gsub('#','\n',x)
}

# same thing but with dplyr and plot 
usa %>% 
  select(mob_vars) %>%
  summarise_all(funs(sum(is.na(.))/dim(usa)[1]*100)) %>% 
  gather() %>% 
    ggplot(aes(y=key, x=value)) + 
    geom_text(aes(label = paste(format(value, digits = 3), "%")), hjust = -0.2, size=5, colour='gray40')+
    geom_col(fill="navy") +
    scale_y_discrete(labels=addline_format(rev(mv_names_ord))) + # reverse labels =) 
    labs(y="", x="% of missing values") + 
    xlim(c(0, 60)) +
#    theme_ev() + 
    theme(axis.text = element_text(
      color = rgb(105, 105, 105, maxColorValue = 255),
      size = 12))    
ggsave('imgs/hist_missing.png', width=8.5, height=4, units='in')  


n_counties = length(unique(usa$fips))

# try plotting everything 
counties_sf %>%
  ggplot() + geom_sf()


#theme_set(theme_bw())

# group by fips, count !!! NON-missing and plot on the map 
usa %>% 
  select(fips, mob_vars) %>% 
  group_by(fips) %>%
  summarise_all(funs(sum(!is.na(.))/365*100)) %>% 
  left_join(counties_sf, by=(c("fips"="county_fips"))) %>%
  pivot_longer(cols=all_day_ratio_single_tile_users:sg_sheltered, names_to = "variables", values_to = "values") %>%
  st_as_sf() %>%
  ggplot() +
  labs(fill = "Complete records, %") +
  facet_wrap(~variables, labeller = as_labeller(var_names, default=label_wrap_gen(25))) + 
  geom_sf(mapping=aes(fill=values), colour=NA) + 
  scale_fill_distiller(palette="Blues", direction=1, na.value='grey90', 
                       guide = guide_colorbar(
                         direction = "horizontal", 
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(100, units = "mm"), 
                       title.position = 'top')) + 
  geom_sf(data=states_sf, fill=NA, colour='white') + 
  theme_minimal() + 
  theme( 
    legend.position="bottom", 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=8)) -> g2

#ggsave('imgs/maps_missing3.png', width=8.5, height=4, units='in', dpi=100)
png("imgs/man_maps.png", width=1600, height=1200, res=150)
print(g2)
dev.off()



# axis.text = element_text(
# color = rgb(105, 105, 105, maxColorValue = 255), size = 12)
