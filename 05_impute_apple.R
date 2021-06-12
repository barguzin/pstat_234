library(softImpute)
library(tidyverse)
library(urbnmapr)
library(sf)

# THIS IMPUTATION FAILED 
##---------------------------------------------------------------------------------------------------------------------------------------------------
# read spatial data with counties and states 
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
states_sf <- get_urbn_map(map = "states", sf = TRUE)

# read in data
usa = read_csv('data/mobility_subset.zip', col_types = cols(apple_mobility = col_double(), 
                                                            fips=col_character())) # for some reason this was reading Apple weird -> specify

# add leading zeroes 
add_lead_zero = function(x) {
  ifelse(nchar(x)==4, paste0('0', x), x)
}

usa$fips = add_lead_zero(usa$fips)


##---------------------------------------------------------------------------------------------------------------------------------------------------
# subset data to only one month only May 
# states: TX, NM, OK, LA, AR, MO, KS, CO, WY, MT, ND, SD, MN, IA, NE
# fips: 48, 35, 40, 22, 05, 29, 20, 08, 56, 30, 38, 46, 27, 19
fips_codes = c('48', '35', '40', '22', '05', '29', '20', '08', '56', '30', '38', '46', '27', '19', '31')
#fips_codes = as.character(fips_codes)
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

##---------------------------------------------------------------------------------------------------------------------------------------------------
usa %>% 
  filter(ds_m4>'2020-05-01' & ds_m4<'2020-05-31') %>% 
  filter(stringr::str_extract(fips, "^.{2}") %in% fips_codes) -> sub_usa


##---------------------------------------------------------------------------------------------------------------------------------------------------

##### softimpute #######

# scale matrix 
sub_usa = scale(sub_usa)

#sub_impute = softImpute(as.matrix(sub_usa %>% select(all_of(mob_vars))), trace=TRUE, type="svd")
sub_impute = softImpute(as.matrix(sub_usa %>% select(all_of(mob_vars))), trace=TRUE, lambda = 2, type="svd", scale=T, unscale=T)

# compute the factorization
sub_usa_imp <- si_usa$u%*% diag(si_usa$d)%*% t(si_usa$v)

dim(sub_usa_imp)

# bind imputed apple to the fips and ds_m4 
bound_apple = bind_cols(sub_usa[, c("fips", "ds_m4", "apple_mobility")], sub_usa_imp[,3])
colnames(bound_apple) = c("fips", "ds_m4", "AppleMobility", "ImputedApple")

bound_apple %>% 
  filter(is.na(AppleMobility)) %>%
  head(10)

# plot 
bound_apple %>% 
  #select(fips, apple_mobility) %>% 
  filter(ds_m4=='2020-05-15') %>%
  group_by(fips) %>%
  summarise(across(where(is.numeric), mean, na.rm=T)) %>%
  left_join(counties_sf, by=(c("fips"="county_fips"))) %>%
  pivot_longer(cols=AppleMobility:ImputedApple, names_to = "variables", values_to = "values") %>%
  st_as_sf() %>%
  ggplot() +
  labs(fill = "Mean Standardized Mobility (source: Apple)") +
  facet_wrap(~variables) + #, labeller = as_labeller(var_names, default=label_wrap_gen(25))) + 
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
    axis.text = element_text(size=8)) -> g3
g3
png("imgs/imputed_failed_apple.png", width=1600, height=1200, res=150)
print(g3)
dev.off()
##---------------------------------------------------------------------------------------------------------------------------------------------------

