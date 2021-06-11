# Purpose of this script is to 
# 1) identify patterns in missing data,
# 2) test the MCAR assumption with diagnostic tests
# 3) use multiple imputation to impute missing values

#### Load packages ####
library(tidyverse)
library(mice)
library(missForest)
library(naniar) # for gg_miss_fct
library(finalfit) # for missing_pairs
library(VIM) # for aggr

# install archived packages
install_archived <- function(url){
  pkgFile <- str_extract(url, "([^/]*)$")
  download.file(url = url, destfile = pkgFile)
  install.packages(pkgs=pkgFile, type="source", repos=NULL)
}

# install_archived("https://cran.r-project.org/src/contrib/Archive/BaylorEdPsych/BaylorEdPsych_0.5.tar.gz")
library(BaylorEdPsych)
# install_archived("https://cran.r-project.org/src/contrib/Archive/mvnmle/mvnmle_0.1-11.1.tar.gz")
library(mvnmle)
# install_archived("https://cran.r-project.org/src/contrib/Archive/MissMech/MissMech_1.0.2.tar.gz")
library(MissMech) # for TestMCARNormality

#### load data ####
usa = read_csv('data/mobility_subset.csv', col_types = cols(apple_mobility = col_double(), 
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

usa %>% 
  filter(ds_m4>'2020-05-01' & ds_m4<'2020-05-31') %>% 
  filter(stringr::str_extract(fips, "^.{2}") %in% fips_codes) -> sub_usa

# Create dataset with 10% randomly missing values
set.seed(25)
usa.mis <- prodNA(sub_usa %>% 
                    select(miles_per_person_umd, staying_home_umd, sg_sheltered, sg_avg_dist2), 
                  noNA = 0.1)
usa.mis = scale(usa.mis)
summary(usa.mis)

# Create dataset with 10% NON-randomly missing values
usa.mis.nonran <- sub_usa %>% 
  select(miles_per_person_umd, staying_home_umd, sg_sheltered, sg_avg_dist2)
usa.mis.nonran[c(1:3666),] <- NA
usa.mis.nonran = scale(usa.mis.nonran)
summary(usa.mis.nonran)

#### Identify missing values ####
# Count missing values by variable
map(sub_usa, ~sum(is.na(.)))

# extract indices of generated missing values 
# get uniq and subset all rows 
tbl_miss = as.data.frame(which(is.na(usa.mis), arr.ind=TRUE))
uniq_rows = unique(tbl_miss$row)

#### visualize missing patterns ####
# real missing values
pdf("./imgs/real_missing_pattern.pdf", width = 10, height = 6)
md.pattern(sub_usa, rotate.names = T)
dev.off()

gg_miss_fct(sub_usa, ds_m4) + 
  ggsave("./imgs/real_missing_fct.pdf", device = "pdf", width = 10, height = 6, units = "cm", scale = 2)

pdf("./imgs/real_missing_prop.pdf", width = 10, height= 6)
aggr_plot <- aggr(sub_usa, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(sub_usa), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
dev.off()

# sub_usa %>% 
#   dplyr::select(c("all_day_ratio_single_tile_users",
#                   "m50", "cases", "deaths", "apple_mobility")) %>%
#   missing_pairs(position = "fill")

# MCAR values
pdf("./imgs/mcar_missing_pattern.pdf", width = 10, height = 6)
md.pattern(bind_cols(usa.mis, ds_m4 = sub_usa$ds_m4), rotate.names = T)
dev.off()

gg_miss_fct(bind_cols(usa.mis, ds_m4 = sub_usa$ds_m4), ds_m4)+ 
  ggsave("./imgs/mcar_missing_fct.pdf", device = "pdf", width = 10, height = 6, units = "cm", scale = 2)

pdf("./imgs/mcar_missing_prop.pdf", width = 10, height= 6)
aggr_plot <- aggr(bind_cols(usa.mis, ds_m4 = sub_usa$ds_m4), col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=c(names(usa.mis), "ds_m4"), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
dev.off()

# missing_pairs(bind_cols(usa.mis, ds_m4 = sub_usa$ds_m4))

# MNAR values
pdf("./imgs/mnar_missing_pattern.pdf", width = 10, height = 6)
md.pattern(bind_cols(usa.mis.nonran, ds_m4 = sub_usa$ds_m4), rotate.names = T)
dev.off()

gg_miss_fct(bind_cols(usa.mis.nonran, ds_m4 = sub_usa$ds_m4), ds_m4)+ 
  ggsave("./imgs/mnar_missing_fct.pdf", device = "pdf", width = 10, height = 6, units = "cm", scale = 2)

pdf("./imgs/mnar_missing_prop.pdf", width = 10, height= 6)
aggr_plot <- aggr(bind_cols(usa.mis.nonran, ds_m4 = sub_usa$ds_m4), col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=c(names(usa.mis.nonran), "ds_m4"), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
dev.off()

# missing_pairs(bind_cols(usa.mis.nonran, ds_m4 = sub_usa$ds_m4))

#### Diagnostic tests for MCAR vs. MAR
# Check for associations between missing and observed data

little_result <- BaylorEdPsych::LittleMCAR(sub_usa[,1:7])
little_result$chi.square
little_result$df
little_result$p.value
little_result_random_mis <- BaylorEdPsych::LittleMCAR(bind_cols(usa.mis, ds_m4 = sub_usa$ds_m4))
little_result_random_mis$chi.square
little_result_random_mis$df
little_result_random_mis$p.value
little_result_nonrandom_mis <- BaylorEdPsych::LittleMCAR(bind_cols(usa.mis.nonran, ds_m4 = sub_usa$ds_m4))
little_result_nonrandom_mis$chi.square
little_result_nonrandom_mis$df
little_result_nonrandom_mis$p.value

# MissMech for missing data pattern tests
# The test of MCAR follows the methodology proposed by Jamshidian and Jalal (2010):
# First, multiple imputation is used based on all observations that have some 
# non-missing values.
# Second, based on the assumption that the imputed values have the same covariances
# two tests are conducted: one with the assumption of normality, another
# non-parametric. H0 is based on testing equality of covariances between groups 
# having identical missing data patterns, i.e. identical covariances.

MissMech::TestMCARNormality(sub_usa[, 3:7])
# MissMech::TestMCARNormality(usa.mis) # vector memory exhausted
# MissMech::TestMCARNormality(usa.mis.nonran) # doesn't run because there are no missing patterns 
# MissMech::TestMCARNormality(bind_cols(usa.mis, ds_m4 = sub_usa$ds_m4))

##### TRYING OUT MICE #####
# mice function to visualize missing 
# md.pattern(usa.mis)

# impute data with mice 
imp_usa_mice <- mice(usa.mis, m=3, maxit = 30, method = 'pmm', seed = 500, print=F)

IMP <- 0
for(i in 1:3) { 
  IMP <- IMP+mice::complete(imp_usa_mice, i)
}
X.mice  <-  IMP/3

test_mice <- X.mice[uniq_rows,]
true_set <- scale(sub_usa %>% select(miles_per_person_umd, staying_home_umd, sg_sheltered, sg_avg_dist2) %>% slice(uniq_rows))
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