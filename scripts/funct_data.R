getwd()
source("Source_libraries.R")


# ##### Loading functional data #####
# funct_data_pladias <- readxl::read_xlsx("data/Funct_traits_pladias.xlsx", na = "NA")
# View(funct_data_pladias)
# summary(funct_data_pladias)
# ## Are all variables characters?
# all(apply(funct_data_pladias, 2, is.character)) # TRUE
# # Yes they are
# names(funct_data_pladias)
# str(funct_data_pladias)
# head(funct_data_pladias)
# 
# ###################################### COLLAPSING DATA ####################################
# 
# #### MYRMECOCHORY DATA
# 
# ## There are some sets of logical columns that can be grouped into one. For example:
# funct_data_pladias[2, str_detect(funct_data_pladias, "[Mm]yrme")]
# #  These are all perfectly collapsable into one saying Myrmechocorous: true or false, even
# #  if we lose some detail.
# ## Let's select the variables that are NON-myrmecochorous, and then we'll do its
# #  complementary set with the inverse of the applied sum.
# ## As of now, subset the raw data
# non_myrm <- function(df = funct_data_pladias, x = "non\\-my"){
#   y <<- apply(df[2, ], 2, str_detect, x)
#   return(y)
#   }
# y[y == T & !is.na(y)]
# 
# non_myrm_data <- funct_data_pladias[, names(non_myrm()[non_myrm() == T & !is.na(non_myrm())])]
# non_myrm_data %<>%
#   summarise(
#     across(
#       .cols = everything(),
#       .fns = as.logical
#     )
#   )
# which(apply(non_myrm_data, 1, sum, na.rm = T) > 1) # 1366
# funct_data_pladias[1367, "species_name"]
# # species 1366: Ranunculus auricommunis agg.
# non_myrm_data[1366, ] 
# # In fact, it has a True for 144 and 146, so its value is 2
# # We'll skip over this detail, and let's go straight to the function. If
# # needed we'll change that single value manually
# myrm_collapsing <- function(df = non_myrm_data){
#   t <- apply(apply(df, 1, as.logical), 2, sum) # Compute if each species has some T for non-myrmecochory
#   return(!as.logical(t)) # use the negative as we are using the variables for NON myrmecochory
# }
# funct_data_pladias$myrm_data <- myrm_collapsing()
# funct_data_pladias$myrm_data[1366] <- FALSE # Correct for Ranunculus auricommunis
# 
# 
# #### NITROGEN FIXATION DATA
# 
# nitrosymbio_data <- funct_data_pladias[, str_detect(names(funct_data_pladias), "[Nn]itrogen")]
# nitrosymbio_data %<>%
#   summarise(
#     across(
#       .fns = as.logical
#     )
#   )
# list_nitro <- apply(nitrosymbio_data[, 1:2], 2, which)
# cond_nitro <- c(list_nitro[[1]], list_nitro[[2]])
# nitro_data <- vector(mode = "logical", length = nrow(funct_data_pladias))
# nitro_data[cond_nitro] <- T

## have we missed some value?
#sum(
#  sum(as.logical(funct_data_pladias$`Symbiotic nitrogen fixation...175`), na.rm = T), 
#  sum(as.logical(funct_data_pladias$`Symbiotic nitrogen fixation...176`), na.rm = T)
#  ) == sum(nitro_data)
## No, everything is fine

#  So, nitro_data can already work as a variable for nitrogen symbionts
# funct_data_pladias$nitro_data <- nitro_data
# funct_data_pladias$nitro_data[1:3] <- NA # Correct the first 3 rows which are headers
# 
# write_csv(funct_data_pladias, file = "./data/correct_funct_data_pladias.csv")
# read_csv("./data/correct_funct_data_pladias.csv") %>% View


#################################### SEARCHING FOR NAs ##################################################

funct_data_pladias <- read_csv("./data/correct_funct_data.csv")
dim(funct_data_pladias)
sum(is.na(funct_data_pladias)) ## In total there are 47770 missing values
which(complete.cases(funct_data_pladias))
sum(complete.cases(funct_data_pladias)) 
## There are 0 rows with no NAs
## There are tons of NAs. Let's create a function to tell us what proportion (%)
#  each column has filled with NAs
## To begin with, the first 3 rows and the first 6 columns 
#  are uninformative to identify NAs and can be misleading

 ### UPDATE 07-11: There's a repeated Cardamine amara entry
# funct_data_pladias
# which(funct_data_pladias$species_name == "Cardamine amara")
# funct_data_pladias[c(285, 291), ]
# any(funct_data_pladias[285, ] != funct_data_pladias[291, ], na.rm = T) # They are exact copies of each other
# funct_data_pladias <- funct_data_pladias[-291, ]


funct_data_pladias2 <- funct_data_pladias[-c(1:3), -c(1:6)]
rownames(funct_data_pladias2) <- funct_data_pladias$species_name[-c(1:3)]
rownames(funct_data_pladias2)

## Now we can create the function:
perc_na <- function(df){
  df %>% summarise(
    across(
      .cols = everything(), # Default option, won't repeat it again
      .fns = as.double
    )
  ) %>%
    summarise(
      across(
        .fns = is.na
      )
    ) %>%
    summarise(
      across(
        .fns = sum
      )
    ) %>%
    summarise(
      across(
        .fns = function(.){./1815*100}
      )
    )
}
perc_na(funct_data_pladias)
## Let's select the columns with less than 30% of missing values
funct_data_pladias2[, which(perc_na(funct_data_pladias2) < 30)]
#  Only 9 variables. Is this real?
## No, the transformation in as.double has been meaningless in many columns.
#  As of now, let's save the percentages for quantitative variables in an object
na_dbl <- funct_data_pladias2[, which(perc_na(funct_data_pladias2) != 100)] 
#  By setting this threshold we capture all the quantitative variables
#  Let's also rename the function:
dbl_perc_na <- perc_na
all(dbl_perc_na(funct_data_pladias2) == perc_na(funct_data_pladias2))
#  Everything's ok
dbl_nas <- dbl_perc_na(na_dbl)

## We have had some troubles with variables which are in essence logical, but
#  codified as characters.
as.double(funct_data_pladias2$`Growth form...13`) # everything is NA.
#  However, by doing:
as.double(as.logical(funct_data_pladias2$`Growth form...13`))
#  we get what we wanted

## Now let's define a function for the columns of logical type.
## We just have to add an extra step to our previous function 
#  first doing an as.logical transformation. If we do:
logicals_perc_na <- function(df){
  df %>% summarise(
    across(
      .cols = everything(), # Default option, won't repeat it again
      .fns = as.logical
    )
  ) %>% summarise(
    across(
      .cols = everything(),
      .fns = as.double 
    )
  ) %>%
    summarise(
      across(
        .fns = is.na
      )
    ) %>%
    summarise(
      across(
        .fns = sum
      )
    ) %>%
    summarise(
      across(
        .fns = function(.){./1815*100}
      )
    )
}

View(logicals_perc_na(funct_data_pladias2))

## And now, by knowing which columns have a 100% NAs by the previous function 
#  we know to what columns we should apply this new one
na_logicals <- logicals_perc_na(funct_data_pladias2[, which(dbl_perc_na(funct_data_pladias2) == 100)])
na_logicals %>% View # 163 cols

na_global <- list(dbl_nas = dbl_nas, dbl_logicals = na_logicals[, -c(1:4)])
## Now we can access the NA percentage of any variable:
na_global[[1]]["Height...7"]
#  Also in vectorized way.
na_global[[2]] %>% View
na_global[[1]] %>% View
## Now let's just gather in an object the names of the variables that have less
#  than 35% of NAs
lt35na <- c(names(na_global[[1]])[which(na_global[[1]] < 35)],
            names(na_global[[2]])[which(na_global[[2]] < 35)]) 
lt35na
# Tries with universal function for NAs -----------------------------------

## We can aslo create a universal function for NAs
na_universal <- function(df){
  df %>%
    summarise(
      across(
        .cols = everything(),
        .fns = as.numeric
      )
    ) %>%
    summarise(
    across(
      .cols = everything(), # Default option, won't repeat it again
      .fns = as.logical
      )
    # ) %>% summarise(
    # across(
    #   .cols = everything(),
    #   .fns = as.double
    #   )
    ) %>%
    plyr::summarise(
      across(
        .fns = is.na
      )
    ) %>%
    plyr::summarise(
      across(
        .fns = sum
      )
    ) %>%
    plyr::summarise(
      dplyr::across(
        .fns = function(.){./1816*100}
      )
    )
}

View(na_universal(funct_data_pladias2))



###################################### FILTERING THE DATA ###############################################

## Let's go back to our original funct_data_pladias
names(funct_data_pladias)


## Preliminarily we could be interested in height, growth form, life form, seed mass, 
#  diaspore and dispersal

####### HEIGHT DATA
height_data <- funct_data_pladias2[, str_detect(names(funct_data_pladias2), "Height")]
head(height_data)
str(height_data) ## All cols are numeric in nature. Let's transform them
height_data

## We had a trouble with the values, as we were using funct_data_pladias and not funct_data_pladias2
#  to create height_data
dbl_perc_na(height_data)
# Only the first 2 columns are not full of NAs, let's drop them
height_data %<>%
  summarise(
    across(
    .cols = 1:2,
    .fns = as.numeric
    )
  )
str(height_data)

## Create a variable for range (max - min)
height_data$range <- height_data$Height...8 - height_data$Height...7
height_data
## And at last let's change the names of the variables according to the spreadsheet
names(height_data) <- c("Min_plant_height", "Max_plant_height", "Range_plant_height")
height_data
## Plotting height data
#  Min height
height_data  %>% 
  ggplot(mapping = aes(x = Min_plant_height)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(limits = c(0, 20))

# Max height
height_data %>%
  ggplot(mapping = aes(x = Max_plant_height)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(0, 10))

# Range height
height_data %>%
  ggplot(mapping = aes(x = Range_plant_height)) +
  geom_histogram(bins = 50)
height_data

## This is easier done in a for loop
for(i in 1:ncol(height_data)){
  height_data %>%
  ggplot(aes(x = height_data[[i]])) +
    geom_histogram(bins = 50) +
    xlab(names(height_data)[i]) +
    scale_x_continuous(limits = c(0, 30)) -> p
    print(p)
  ggsave(filename = paste0(names(height_data)[i],".png"), path = "./plots/histograms")
}

## No height variable has normal distribution
logicals_perc_na(dispersal_data)

#### SEED DATA
funct_data_pladias[2, str_detect(names(funct_data_pladias), "Seed")]

seed_data <- funct_data_pladias2[, str_detect(names(funct_data_pladias2), "Seed")]
head(seed_data)
names(seed_data)
View(seed_data)
## All are in essence quantitative variables, so we'll use dbl_perc_na()
dbl_perc_na(seed_data)
## 3rd, 4th and 6th cols are only NAs. Let's drop them. Besides, before
#  before plotting, we have to transform the variables to numeric 
seed_data %<>%
  summarise(
    across(
      .cols = c(1, 2, 5),
      .fns = as.double
    )
  )
seed_data
## And a last step is to assign the names, according to the spreadsheet
names(seed_data) <- c("Min_seed_mass", "Max_seed_mass", "Mean_seed_mass")

### Plotting seed data:
for(i in 1:ncol(seed_data)){
  ggplot(data = seed_data, mapping = aes(x = seed_data[[i]])) +
    geom_histogram(bins = 30) +
    xlab(label = str_c(names(seed_data)[i])) +
    scale_x_continuous(limits = c(0, 1000)) +
    scale_y_continuous(limits = c(0, 75))-> p
  print(p)
  ggsave(filename = paste0(names(seed_data[i]), ".png"), path = "./plots/histograms")
}
## No seed variable has a normal distribution


#### LEAF DATA
leaf_data <- funct_data_pladias2[, str_detect(names(funct_data_pladias2), "[Ll]eaf")]
## We are not interested in anatomy, metamorphosis or rosette
leaf_data <- leaf_data[, !str_detect(names(leaf_data), "anatomy|metamorphosis|rosette")]
leaf_data
## We have 2 types of data, logical and numeric (continuous) 
#  Select logical variables, which are for the levels of life span
leaf_data_lgcl <- leaf_data %>%
  select_if(str_detect(names(leaf_data), "life span"))
## Test NA abundance
logicals_perc_na(leaf_data_lgcl) # All are under the threshold
## Transform variable type
leaf_data_lgcl %<>%
  summarise(
    across(
      .cols = everything(),
      .fns = as.logical
    )
  )

names(leaf_data_lgcl) <- funct_data_pladias[2, str_detect(names(funct_data_pladias), "Leaf life span")]
names(leaf_data_lgcl)
names(leaf_data_lgcl) %<>% 
  str_replace_all("\\s", "_")
names(leaf_data_lgcl)

## Select numerical variables (complementary set of logical variables)
leaf_data_num <- leaf_data %>%
  select_if(!str_detect(names(leaf_data), "life span"))
## Transform variable type
leaf_data_num %<>%
  summarise(
    across(
      .cols = everything(),
      .fns = as.numeric
    )
  )
dbl_perc_na(leaf_data_num)

## Plotting numerical leaf data
#  First change the names to replace white spaces
names(leaf_data) <- str_replace_all(names(leaf_data), pattern = "\\s", replacement  = "_")
names(leaf_data_num) <- str_replace_all(names(leaf_data_num), pattern = "\\s", replacement = "_")

for(i in 1:ncol(leaf_data_num)){
leaf_data_num %>%
  ggplot(aes(x = leaf_data_num[[i]])) +
  geom_histogram() +
  xlab(label = str_c(names(leaf_data_num)[i])) -> p
  print(p)
  ggsave(filename = paste0(names(leaf_data_num)[i], ".png"), path = "./plots/histograms")
  }

## Only Leaf dry matter content has a relatively normal distribution. Let's test it

pval <- vector(mode = "numeric", length = 1000)
for (i in 1:length(pval)){
  x <- rnorm(1000, mean = mean(leaf_data_num$Leaf_dry_matter_content, na.rm = T),
           sd = sd(leaf_data_num$Leaf_dry_matter_content, na.rm = T))
  pval[i] <- ks.test(leaf_data_num$Leaf_dry_matter_content, x)$p.value
}
mean(pval) # After 1000 simulations, the mean p-value of the Kolmogorov-Smirnov test 
# is 0.017 so we reject its normality



#### LIFE FORMS

funct_data_pladias[2, str_detect(names(funct_data_pladias), "[Ll]ife form")] # get the real levels of life forms
life_form <- funct_data_pladias2[, str_detect(names(funct_data_pladias2), "[Ll]ife form")]
## Let's rename the variables according to the levels
for(i in 1:ncol(life_form)){
names(life_form)[i] <- as.data.frame(funct_data_pladias)[2, str_detect(names(funct_data_pladias), "[Ll]ife form")][1, i]
}
names(life_form)

## As all the variables are logical in essence, let's transform them:
life_form %<>%
  summarise(
    across(
      .fns = as.logical
    )
  )
life_form
logicals_perc_na(life_form) # very few NAs


#### PARASITISM DATA
funct_data_pladias[2, str_detect(names(funct_data_pladias), "[Pp]arasitism")] %>% View
parasitism_data <- funct_data_pladias2[, str_detect(names(funct_data_pladias2), "[Pp]arasitism")]
parasitism_data %<>%
  summarise(
    across(
      .fns = as.logical
      )
    )
logicals_perc_na(parasitism_data) # 0 NAs in these variables
parasitism_data

#### DISPERSAL DATA
funct_data_pladias[2, str_detect(names(funct_data_pladias), "[Dd]ispersal")] %>% View

dispersal_data <- funct_data_pladias2
dispersal_data %<>%
  select_if(str_detect(names(dispersal_data), "[Dd]ispersal"))

logicals_perc_na(dispersal_data) %>% View  # Very few NAs

dispersal_data %<>%
  summarise(
    across(
      .fns = as.logical
    )
  )
dispersal_data
## This subset of data contains many variables (21), perhaps we should subset again...
apply(dispersal_data, 2, sum, na.rm = T)
#  Dispersal unit (diaspore)...128 has all FALSE values (its sum is 0) and this can be problematic,
#  let's remove it
dispersal_data$`Dispersal unit (diaspore)...128` <- NULL


################################## TRANSFORMING THE DATA #####################################

########## LOGARITHMIC TRANSFORMATION

#### HEIGHT DATA
log_height_data <- log(height_data)
log_height_data
for(i in 1:ncol(log_height_data)){
names(log_height_data)[i] <- paste0("Log_", names(log_height_data)[i])
}
names(log_height_data) # All have been updated
for (i in 1:ncol(log_height_data)){
  log_height_data %>%
    ggplot(aes(x = log_height_data[[i]])) +
    geom_histogram() +
    xlab(names(log_height_data[i])) -> p
  print(p)
  ggsave(filename = paste0(names(log_height_data)[i], ".png"), path = "./plots/histograms")
}
## Now they resemble much more a normal distribution
max(seed_data$Max_seed_mass, na.rm = T)
funct_data_pladias
#### SEED DATA
log_seed_data <- log(seed_data)
log_seed_data
for(i in 1:ncol(log_seed_data)){
  names(log_seed_data)[i] <- paste0("Log_", names(log_seed_data)[i])
}
names(log_seed_data) # All have been updated
for (i in 1:ncol(log_seed_data)){
  log_seed_data %>%
    ggplot(aes(x = log_seed_data[[i]])) +
    geom_histogram() +
    xlab(names(log_seed_data[i])) -> p
  print(p)
  ggsave(filename = paste0(names(log_seed_data)[i], ".png"), path = "./plots/histograms")
}


#### LEAF DATA
log_leaf_data <- log(leaf_data_num)
for(i in 1:ncol(log_leaf_data)){
  names(log_leaf_data)[i] <- paste0("Log_", names(log_leaf_data)[i])
}
names(log_leaf_data)

for(i in 1:ncol(log_leaf_data)){
  log_leaf_data %>%
  ggplot(aes(x = log_leaf_data[[i]])) +
    geom_histogram() +
    xlab(names(log_leaf_data[i])) -> p
  print(p)
  ggsave(filename = paste0(names(log_leaf_data)[i], ".png"), path = "./plots/histograms")
}
## Log_leaf_size doesn't seem so much normal. Let's test it with Kolmogorov-Smirnov test

pval <- vector(mode = "numeric", length = 1000)
for(i in 1:length(pval)){
  x <- rnorm(1000, mean = mean(log_leaf_data[["Log_Leaf_size"]], na.rm = T),
             sd = sd(log_leaf_data[["Log_Leaf_size"]], na.rm = T))
  pval[i] <- ks.test(log_leaf_data[["Log_Leaf_size"]], x)$p.value
}
mean(pval) # It has a mean p-value of 0.29 after 1000 iterations so it's acceptable as normal


####################################### PCA #############################################

## Let's fuse all the log-transformed variables
names(funct_data_pladias)
log_data <- cbind(log_height_data, log_seed_data, log_leaf_data, leaf_data_lgcl, life_form)
                  #parasitism_data, dispersal_data) # Added this 2 sets of variables in 25/10
log_data %>% View
funct_data_pladias %>% View
nrow(log_data)
## Centering the data 
?scale
centered_log_data <- apply(log_data, 2, scale)
centered_log_data %>% View
apply(centered_log_data, 2, mean, na.rm = T) # They all have mean = 0
?prcomp
## We can always center with scale. = T in the arguments of prcomp() but it's nice
#  to test if the results are the same.

## Scaled PCA
pca_log_data_sc <- prcomp(log_data[complete.cases(log_data), ], 
                          scale. = T)

summary(pca_log_data_sc) # First 2 axes don't explain the 40% of variance
plot(pca_log_data_sc, type = "l")
biplot(pca_log_data_sc) ## Very orthogonal disposition

#  The next PCA is done with the data scaled manually
pca_log_data_sc2 <- prcomp(centered_log_data[complete.cases(centered_log_data), ])

pca_log_data_sc2$x

summary(pca_log_data_sc2)
plot(pca_log_data_sc2, type = "l")
biplot(pca_log_data_sc2)
#  The results are almost identical

## Non Scaled PCA
pca_log_data_nsc <- prcomp(log_data[complete.cases(log_data), ])
summary(pca_log_data_nsc) # First 2 axes explain more than 80% of variance
plot(pca_log_data_nsc, type = "l")
biplot(pca_log_data_nsc)

## Alternative visualizations:
names(funct_data_pladias)
#  devtools::install_github("vqv/ggbiplot")

#  Non-scaled PCA:
ggbiplot::ggbiplot(pca_log_data_nsc)
ggbiplot::ggscreeplot(pca_log_data_nsc)

#  Scaled PCA
ggbiplot::ggbiplot(pca_log_data_sc)
ggbiplot::ggscreeplot(pca_log_data_sc)
ggbiplot::ggbiplot(pca_log_data_sc2)
ggbiplot::ggscreeplot(pca_log_data_sc2)
pca3d::pca3d(pca_log_data_sc2)
pca_log_data_sc$rpt[, 1]


################################ PCA ON SUBSETTED DATA ##############################
correct_funct_data_pladias <- read_csv("./data/correct_funct_data_pladias.csv")
sub_log_data <- cbind(log_height_data$Log_Max_plant_height, log_seed_data$Log_Mean_seed_mass, log_leaf_data, 
                      leaf_data_lgcl, life_form,
                      parasitism_data, dispersal_data,
                      correct_funct_data_pladias$myrm_data[-c(1:3)],
                      correct_funct_data_pladias$nitro_data[-c(1:3)])
rownames(sub_log_data) <- rownames(funct_data_pladias2)
apply(sub_log_data, 2, var, na.rm = T)
## Scaled PCA
sub_log_data_sc <- scale(sub_log_data)

pca_sub_log_data_sc <- prcomp(sub_log_data_sc[complete.cases(sub_log_data_sc), ], 
                          scale. = F)

summary(pca_sub_log_data_sc) # First 2 axes don't explain the 40% of variance
plot(pca_sub_log_data_sc, type = "l")
biplot(pca_sub_log_data_sc) ## Very orthogonal disposition
ggbiplot::ggbiplot(pca_sub_log_data_sc)
ggbiplot::ggbiplot(pca_sub_log_data_sc, choices = c(1,3))
ggbiplot::ggbiplot(pca_sub_log_data_sc, choices = c(2,3))
## It seems that LDMC is very difficult to classify in any of the PC spaces
ggbiplot::ggscreeplot(pca_sub_log_data_sc)

spp_scores <- as.data.frame(pca_sub_log_data_sc$x)# %>%
spp_scores[[1]]
rownames(spp_scores)
spp_scores$PC1

# Plotting the species from the PCA scores --------------------------------

spp_scores_test <- cbind(rownames(spp_scores), spp_scores$PC1, spp_scores$PC2,
                         spp_scores$PC3)

colnames(spp_scores_test) <- c("Species_name", "PC1", "PC2", "PC3")

spp_scores_test <- as_tibble(spp_scores_test)

spp_scores_test %<>%
   summarise(
     across(
       .cols = c(2:4),
       .fns = as.double
     )
   ) %>%
  mutate(
    species_name = spp_scores_test$Species_name
  ) %>%
  select(species_name, everything())

spp_scores_test


######### TOP 25 SPECIES PLOTTING
### Order the species by rank in PC1
spp_scores_test <- spp_scores_test[order(spp_scores_test$PC1, decreasing = T), ]
spp_scores_test[c(1:25), ] %>%
  ggplot(mapping = aes(x = reorder(species_name, X = PC1), y = PC1)) +
  geom_point() +
  labs(title = "25 highest-ranking species ordered by scores in PC1",
      y = "PC1 score",
      x = "Species name") +
  coord_flip() -> p.maxPC1
print(p.maxPC1)

spp_scores_test[c((nrow(spp_scores_test) - 24) : nrow(spp_scores_test)), ] %>%
  ggplot(mapping = aes(x = reorder(species_name, X =- PC1), y = PC1)) +
  geom_point() +
  labs(title = "25 lowest-ranking species ordered by scores in PC1",
       y = "PC1 score",
       x = "Species name") +
  coord_flip() -> p.minPC1
print(p.minPC1)

### Order the species by rank in PC2
spp_scores_test <- spp_scores_test[order(spp_scores_test$PC2, decreasing = T), ]

spp_scores_test[c(1:25), ] %>%
  ggplot(mapping = aes(x = reorder(species_name, X = PC2), y = PC2)) +
  geom_point() +
  labs(title = "25 highest-ranking species ordered by scores in PC2",
       y = "PC2 score",
       x = "Species name") +
  coord_flip() -> p.maxPC2
print(p.maxPC2)

spp_scores_test[c((nrow(spp_scores_test) - 24) : nrow(spp_scores_test)), ] %>%
  ggplot(mapping = aes(x = reorder(species_name, X = -PC2), y = PC2)) +
  geom_point() +
  labs(title = "25 lowest-ranking species ordered by scores in PC2",
       y = "PC2 score",
       x = "Species name") +
  coord_flip() -> p.minPC2
print(p.minPC2)

### Order the species by rank in PC3
spp_scores_test <- spp_scores_test[order(spp_scores_test$PC3, decreasing = T), ]

spp_scores_test[c(1:25), ] %>%
  ggplot(mapping = aes(x = reorder(species_name, X = PC3), y = PC3)) +
  geom_point() +
  labs(title = "25 highest-ranking species ordered by scores in PC3",
       y = "PC3 score",
       x = "Species name") +
  coord_flip() -> p.maxPC3
print(p.maxPC3)

spp_scores_test[c((nrow(spp_scores_test) - 24) : nrow(spp_scores_test)), ] %>%
  ggplot(mapping = aes(x = reorder(species_name, X = -PC3), y = PC3)) +
  geom_point() +
  labs(title = "25 lowest-ranking species ordered by scores in PC3",
       y = "PC3 score",
       x = "Species name") +
  coord_flip() -> p.minPC3
print(p.minPC3)


############### WHOLE SCORES PLOTTING
spp_scores_maxPC1 <- spp_scores_test[order(spp_scores_test$PC1, decreasing = T), ]
spp_scores_maxPC2 <- spp_scores_test[order(spp_scores_test$PC2, decreasing = T), ] 
spp_scores_maxPC3 <- spp_scores_test[order(spp_scores_test$PC3, decreasing = T), ]

## Plot for PC1
spp_scores_maxPC1 %>%
  ggplot(mapping = aes(x = reorder(species_name, X = PC1), y = PC1)) +
  geom_point(size = 0.5) +
  labs(title = "Total species ordered by scores in PC1",
       y = "PC1 score",
       x = "Species name") +
  geom_rect(aes(
    xmin = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC1) == (1019-24), "species_name"], 
    xmax = Inf,
    ymin = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC1) == (1019-24), "PC1"], 
    ymax = Inf), 
    alpha = 0.005, fill = "blue") +
  geom_rect(aes(
    xmin = -Inf, 
    xmax = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC1) == 25, "species_name"],
    ymin = -Inf, 
    ymax = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC1) == 25, "PC1"]), 
  fill = "red", alpha = 0.005) +
  annotate(geom = "text", 
           x = as.data.frame(
             spp_scores_maxPC1)[80, 1], 
           y = mean(head(spp_scores_maxPC1$PC1, 25)),
           label = "25 highest-scored spp") +
  annotate(geom = "text", 
           x = as.data.frame(
             spp_scores_maxPC1)[(nrow(spp_scores_maxPC1) - 80), 1],
           y = mean(tail(spp_scores_maxPC1$PC1, 25)),
           label = "25 lowest-scored spp") + 
  coord_flip()
# Weird looking sigmoidal curve

## Plot for PC2
spp_scores_maxPC2 %>%
  ggplot(mapping = aes(x = reorder(species_name, X = PC2), y = PC2)) +
  geom_point(size = 0.5) +
  labs(title = "Total species ordered by scores in PC2",
       y = "PC2 score",
       x = "Species name") +
  geom_rect(aes(
    xmin = as.data.frame(
    spp_scores_test)[rank(spp_scores_test$PC2) == (1019-24), "species_name"], 
    xmax = Inf,
    ymin = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC2) == (1019-24), "PC2"], 
    ymax = Inf), 
    alpha = 0.005, fill = "blue") +
  geom_rect(aes(
    xmin = -Inf, 
    xmax = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC2) == 25, "species_name"],
    ymin = -Inf, 
    ymax = as.data.frame(
            spp_scores_test)[rank(spp_scores_test$PC2) == 25, "PC2"]), 
    fill = "red", alpha = 0.005) +
  annotate(geom = "text", 
           x = as.data.frame(
             spp_scores_maxPC2)[80, 1], 
           y = mean(head(spp_scores_maxPC2$PC2, 25)),
           label = "25 highest-scored spp")+
  annotate(geom = "text", 
           x = as.data.frame(
             spp_scores_maxPC2)[(nrow(spp_scores_maxPC2) - 80), 1],
           y = mean(tail(spp_scores_maxPC2$PC2, 25)),
           label = "25 lowest-scored spp") +
  coord_flip()
# Very attenuated sigmoidal, almost linear

## Plot for PC3
spp_scores_maxPC3 %>%
  ggplot(mapping = aes(x = reorder(species_name, X = PC3), y = PC3)) +
  geom_point(size = 0.5) +
  labs(title = "Total species ordered by scores in PC3",
       y = "PC3 score",
       x = "Species name") +
  geom_rect(aes(
    xmin = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC3) == (1019-24), "species_name"], 
    xmax = Inf,
    ymin = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC3) == (1019-24), "PC3"], 
    ymax = Inf), 
  alpha = 0.005, fill = "blue") +
  geom_rect(aes(
    xmin = -Inf, 
    xmax = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC3) == 25, "species_name"],
    ymin = -Inf, 
    ymax = as.data.frame(
      spp_scores_test)[rank(spp_scores_test$PC3) == 25, "PC3"]), 
  fill = "red", alpha = 0.005) +
  annotate(geom = "text", 
           x = as.data.frame(
             spp_scores_maxPC3)[80, 1], 
           y = mean(head(spp_scores_maxPC3$PC3, 25)),
           label = "25 highest-scored spp")+
  annotate(geom = "text", 
           x = as.data.frame(
             spp_scores_maxPC3)[(nrow(spp_scores_maxPC3) - 80), 1],
           y = mean(tail(spp_scores_maxPC3$PC3, 25)),
           label = "25 lowest-scored spp") +
  coord_flip()
# A little left-skewed sigmoidal

##### NORMALITY OF PCs ####
for(i in 2:4){
x <- c(NA, "PC1", "PC2", "PC3")
spp_scores_test %>%
  ggplot(mapping = aes(x = .[[i]])) +
  geom_histogram() + 
  labs(title = paste0("Histogram of ", x[[i]]),
       x = x[[i]]) -> p
  print(p)
  ggsave(plot = p, filename = paste0(str_replace_all(p$labels$title, "\\s", "_"), ".png"),
         path = "./plots/histograms")
}

##### LOOKING FOR OVERLAPS IN PCs #######
# Let's see if there's any overlap between extremes of PCs

spp_scores_maxPC1[which(p.maxPC1$plot_env$.$species_name %in% 
                          p.maxPC2$plot_env$.$species_name), 1] -> overlap_max_PC12
overlap_max_PC12 # Veronica agrestis

spp_scores_maxPC1[which(p.maxPC1$plot_env$.$species_name %in% p.maxPC3$plot_env$.$species_name), 1]
# Null overlap

spp_scores_maxPC2[which(p.maxPC2$plot_env$.$species_name %in% p.maxPC3$plot_env$.$species_name), 1]
# Null overlap

tail(spp_scores_maxPC1, 25)[which(p.minPC1$plot_env$.$species_name %in%
                                    p.minPC2$plot_env$.$species_name), 1] -> overlap_min_PC12

tail(spp_scores_maxPC1, 25)[which(p.minPC1$plot_env$.$species_name %in%
                                    p.minPC3$plot_env$.$species_name), 1] -> overlap_min_PC13

tail(spp_scores_maxPC2, 25)[which(p.minPC2$plot_env$.$species_name %in%
                                    p.minPC3$plot_env$.$species_name), 1] -> overlap_min_PC23
overlap_min_PC12 # No overlap
overlap_min_PC13 # big overlap (10/25). What does this mean, if in principle PCs are orthogonal?
overlap_min_PC23 # moderate overlap (4/25)

# Let's see if there are opposed (min vs max and viceversa) overlaps:
spp_scores_maxPC1[which(p.maxPC1$plot_env$.$species_name %in%
                          p.minPC2$plot_env$.$species_name), 1] # max PC1 with min PC2 = 0
spp_scores_maxPC1[which(p.maxPC1$plot_env$.$species_name %in%
                          p.minPC3$plot_env$.$species_name), 1] # max PC1 with min PC3 = 0
spp_scores_maxPC2[which(p.maxPC2$plot_env$.$species_name %in%
                          p.minPC1$plot_env$.$species_name), 1] # max PC2 with min PC1 = 0
spp_scores_maxPC2[which(p.maxPC2$plot_env$.$species_name %in% 
                          p.minPC3$plot_env$.$species_name), 1] # max PC2 with min PC3 = 0
spp_scores_maxPC3[which(p.maxPC3$plot_env$.$species_name %in%
                          p.minPC1$plot_env$.$species_name), 1] # max PC3 with min PC1 = 0
spp_scores_maxPC3[which(p.maxPC3$plot_env$.$species_name %in%
                          p.minPC2$plot_env$.$species_name), 1] # max PC3 with min PC2 = 0
tail(spp_scores_maxPC1, 25)[which(p.minPC1$plot_env$.$species_name %in%
                                    p.maxPC2$plot_env$.$species_name), 1] # min PC1 with max PC2 = 0
tail(spp_scores_maxPC1, 25)[which(p.minPC1$plot_env$.$species_name %in%
                                    p.maxPC3$plot_env$.$species_name), 1] # min PC1 with max PC3 = 0
tail(spp_scores_maxPC2, 25)[which(p.minPC2$plot_env$.$species_name %in%
                                    p.maxPC1$plot_env$.$species_name), 1] # min PC2 with max PC1 = 0
tail(spp_scores_maxPC2, 25)[which(p.minPC2$plot_env$.$species_name %in%
                                    p.maxPC3$plot_env$.$species_name), 1] # min PC2 with max PC3 = 0
tail(spp_scores_maxPC3, 25)[which(p.minPC3$plot_env$.$species_name %in%
                                    p.maxPC1$plot_env$.$species_name), 1] # min PC3 with max PC1 = 0
tail(spp_scores_maxPC3, 25)[which(p.minPC3$plot_env$.$species_name %in%
                                    p.maxPC2$plot_env$.$species_name), 1] # min PC3 with max PC2 = 0
# There are no extra overlaps


################################# ADDING SUCCESSION INDEX DATA #################################
# Upload the data
succ_data <- readxl::read_xlsx("data/Optima_succession_(years).xlsx")

# Let's homogeneize the key for the joining
names(funct_data_pladias)[2] <- "species_name"
names(succ_data)[1] <- "species_name"

funct_data_pladias_total <- left_join(funct_data_pladias, succ_data[, 1:2], by = "species_name")
names(funct_data_pladias_total)[ncol(funct_data_pladias_total)] <- "succ_optimum" 

succ_optimum_vector <- funct_data_pladias_total$succ_optimum
funct_data_pladias_total %>%
  ggplot(aes(x = succ_optimum)) +
  geom_histogram() +
  labs(title = "Histogram of raw succession optima",
       x = "Succession optima (years)")

## Clearly not normal, let's try a Box-Cox transformation (following Shipley, 2021)
out <- MASS::boxcox(succ_optimum_vector~1, plotit = F)
lambda <- out$x[which(out$y == max(out$y))]
lambda

funct_data_pladias_total %>%
  ggplot(aes(x = succ_optimum^lambda)) +
  geom_histogram() +
  labs(title = "Histogram of Box-Cox transformed succession optima",
       x = "Succession optima Box-Cox-transformed (0.3 power)")

## Doesn't work quite well
mean(succ_optimum_vector^lambda, na.rm = T)
sd(succ_optimum_vector^lambda, na.rm = T)
pval2 <- vector(mode = "double", length = 1000)
for (i in 1:length(pval2)){
    x <- rnorm(1000, mean = 2.413,
               sd = 0.666)
    pval2[i] <- ks.test(succ_optimum_vector^lambda, x)$p.value
}
mean(pval2) # 0.02, it's definitely non-normal

## We cannot use the succession index in the PCA. However we might still use it as the
#  dependent variable against the scores from the previous PCA
work_data <- left_join(spp_scores_test, succ_data, by = "species_name")
work_data <- work_data[, -6]  # Drop "Komentar" variable
names(work_data)[ncol(work_data)] <- "succ_optimum" # Change the succession optima names

work_data %>%
  ggplot(mapping = aes(x = PC1, y = succ_optimum)) +
  geom_line() +
  geom_smooth() # This is the only one with a reasonable linear trend (only visible
# in the smoothed curve)

work_data %>%
  ggplot(mapping = aes(x = PC2, y = succ_optimum)) +
  geom_line() +
  geom_smooth()

work_data %>%
  ggplot(mapping = aes(x = PC3, y = succ_optimum)) +
  geom_line() +
  geom_smooth()

# Let's add the data from the Box-Cox transformation
work_data$boxcox_succ <- work_data$succ_optimum^lambda
for(i in 2:4){
  x <- c("PC1", "PC2", "PC3")
  work_data %>%
  ggplot(mapping = aes(x = .[[i]], y = boxcox_succ)) +
  geom_line() +
  geom_smooth() +
  labs(title = paste("Trend of succession optimum against", x[i-1]),
       x = x[[i-1]],
       y = "Box-Cox transform of successional optimum") -> p
  print(p)
  ggsave(plot = p, filename = paste0(str_replace_all(p$labels$title, "\\s", "_"),
                                     ".png"),
         path = "./plots")
  }

## In general the trend does not change after the Box-Cox transform

######################### STATISTICAL MODELS #################################

mod_succ1 <- nlme::gls(boxcox_succ~PC1+PC2+PC3, data = work_data, na.action = na.omit)
update(mod_succ1, data = work_data)
summary(mod_succ1) # PC3 is irrelevant
plot(mod_succ1)
qqnorm(mod_succ1)

mod_succ2 <- nlme::gls(boxcox_succ~PC1+PC2, data = work_data, na.action = na.omit)
summary(mod_succ2)
plot(mod_succ2)
qqnorm(mod_succ2)

## Both PC1 and PC2 are highly significant and with the same trend. 
#  Residuals are not too non-normal


# OUTRO -----------------------------------------------------------
save.image(file = paste0(Sys.Date(), ".RData"))
