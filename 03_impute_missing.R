#library(caret)
#library(tidymodels)
library(tidyverse)
library(sf)
library(missForest)
library(Hmisc)
library(mice)
library(mi)
library(Amelia)
library(softImpute)
library(urbnmapr)
library(doParallel)
library(missMDA)

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

usa %>% 
  filter(ds_m4>'2020-05-01' & ds_m4<'2020-05-31') %>% 
  filter(stringr::str_extract(fips, "^.{2}") %in% fips_codes) -> sub_usa

dim(sub_usa)

# plot 
sub_usa %>% 
  select(fips, mob_vars) %>% 
  group_by(fips) %>%
  summarise_all(funs(sum(!is.na(.))/31*100)) %>% 
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
    axis.text = element_text(size=8)) -> g3
g3

#ggsave('imgs/maps_missing3.png', width=8.5, height=4, units='in', dpi=100)
png("imgs/man_sub_maps.png", width=1600, height=1200, res=150)
print(g3)
dev.off()


# make 10% of data missing 
set.seed(25)
usa.mis <- prodNA(sub_usa %>% select(miles_per_person_umd, staying_home_umd, sg_sheltered, sg_avg_dist2), noNA = 0.1)
usa.mis = scale(usa.mis)
summary(usa.mis)

# extract indices of generated missing values 
# get uniq and subset all rows 
tbl_miss = as.data.frame(which(is.na(usa.mis), arr.ind=TRUE))
uniq_rows = unique(tbl_miss$row)


##### TRYING OUT MICE #####
# mice function to visualize missing 
# md.pattern(usa.mis)

# impute data with mice 
imp_usa_mice <- mice(usa.mis, m=5, maxit = 50, method = 'pmm', seed = 500, print=F)

IMP <- 0
for(i in 1:5) { 
  IMP <- IMP+mice::complete(imp_usa_mice, i)
}
X.mice  <-  IMP/5

# old code
#cc = mice::complete(imp_usa_mice)
#summary(cc)

test_mice = X.mice[uniq_rows,]
true_set = scale(sub_usa %>% select(miles_per_person_umd, staying_home_umd, sg_sheltered, sg_avg_dist2) %>% slice(uniq_rows))
dim(true_set)

# write a function for rmse 
my_rmse = function(test_df, true_df=true_set) {
  rmse = sqrt(mean(as.matrix((true_df - test_df)^2), na.rm=T))
  return(rmse)
}

#mice_rmse = sqrt(mean(as.matrix(true_set - test_mice)^2))
mice_rmse = my_rmse(test_mice)
mice_rmse # 0.61286796 with one sample 
# 0.4734311 with 5 samples per iteration

# cc %>%
#   select(1)%>%
#   slice(tbl_miss %>% filter(col==1) %>% dplyr::pull(row)) -> u1
# cc %>%
#   select(2)%>%
#   slice(tbl_miss %>% filter(col==2) %>% dplyr::pull(row)) -> u2
# cc %>%
#   select(3)%>%
#   slice(tbl_miss %>% filter(col==3) %>% dplyr::pull(row)) -> u3
# cc %>%
#   select(4)%>%
#   slice(tbl_miss %>% filter(col==4) %>% dplyr::pull(row)) -> u4
# 
# 
# bound_usa_mis = bind_cols(u1, u2, u3, u4) # this does not work because there are different number of missing values 
# dim(bound_usa_mis)

##### softimpute #######
si_usa = softImpute(as.matrix(usa.mis), trace=TRUE, type="svd")

# compute the factorization
fac_si_usa <- si_usa$u%*% diag(si_usa$d)%*% t(si_usa$v)

# replace missing values by computed values
test_soft = fac_si_usa[uniq_rows,]

# calculate rmse
soft_rmse = my_rmse(test_soft)
soft_rmse


##### AMELIA ##### 
mama = 10
amelia_fit <- amelia(usa.mis, m=mama, parallel = "multicore")

IMP <- 0
for(i in 1:mama) { 
  IMP <- IMP + amelia_fit$imputations[[i]]
}
X.amelia  <-  IMP/mama

test_amel = X.amelia[uniq_rows,]

summary(test_amel) 
# !!! this method can't do anything about completely missing observations 
# !!! need to drop them

amel_rmse = my_rmse(test_amel)
amel_rmse


##### missFOrest ######
# THIS TAKES FOREVER EVEN IN PARALLEL #
registerDoParallel(cores=4)
# made up example 
#x=matrix(rnorm(1000000),1000,1000)
#x[sample(1:50, 10, replace=FALSE)]=NA
#mf_impute <-missForest(xmis = as.matrix(x), maxiter = 5, ntree = 50, parallelize = "forests")

mf_impute <-missForest(xmis = as.matrix(usa.mis), maxiter = 3, ntree = 25, parallelize = "forests") # takes 5 mins per iteration
dim(mf_impute$ximp)

test_mf = mf_impute$ximp[uniq_rows,]
mf_rmse = my_rmse(test_mf)
mf_rmse

##### MDA imputation ##### 
# estimate number of pca
res.ncp <- estim_ncpPCA(as.matrix(usa.mis), method.cv="Kfold")
#plot(res.ncp$criterion~names(res.ncp$criterion),xlab="number of dimensions")
ncp=3

#res.BayesMIPCA<-MIPCA(X=as.matrix(usa.mis), nboot=100, ncp=ncp, method.mi="Bayes")
pca <-imputePCA(X = as.matrix(usa.mis), ncp = 3, scale = TRUE, method =c("Regularized","EM"))
X.pca <- pca$comp
dim(X.pca)

test_mda = X.pca[uniq_rows,]
mda_rmse = my_rmse(test_mda)
mda_rmse


#### SAVE and PLOT ##### 
write.table(test_mf, 'data/test_mf.csv')
write.table(test_mda, 'data/test_mda.csv')
write.table(test_amel, 'data/test_amel.csv')
write.table(test_soft, 'data/test_soft.csv')
write.table(test_mice, 'data/test_mice.csv')

all_rmse = c(amel_rmse, mda_rmse, mf_rmse, mice_rmse, soft_rmse)

#rmse_df = as.data.frame(all_rmse) 
rmse_df = as.data.frame(cbind(all_rmse, c('Amelia', 'missMDA', 'missForest', 'mice', 'softImpute')))
colnames(rmse_df) = c('val', 'method')
rmse_df$val = as.numeric(as.character(rmse_df$val))


source('src/theme_ev.R')

rmse_df %>%
#  summarise(val = round(val, 2)) %>%
  ggplot(aes(x=method, y=val)) + 
  geom_col(fill='navy') + 
  theme_ev() + 
  labs(x='', y='RMSE') -> g4
png("imgs/rmse_bar.png", width=1600, height=1200, res=150)
print(g4)
dev.off()
