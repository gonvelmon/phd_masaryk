source("Source_libraries.R")
getwd()
## DATA LOADING

#### Functional data #### 
funct_data_pladias <- read_csv("data/traits/correct_funct_data.csv", na = "NA") 

funct_data_depcor <- read_csv("data/traits/traits_transformed_missing.preserved.csv", na = "NA")

new_pladias_na <- read_csv("/data/traits/traits_transformed_missing.preserved.csv")
new_pladias_na %<>%
  rename(species = X)
new_pladias_na$species %<>%
  str_replace_all("_", " ")

#### Composition data ####

## GRASSLANDS
pre_data_gl <- read_delim("./data/composition/Excl_exp_Festuco-Brometea2.txt", delim = ";",
                       col_names = F)

## WHOLE COUNTRY
whole_cz <- read_tsv("data/composition/dataspxp.txt")

## DEVIN SPECIES
spp_devin <- read_csv("./data/composition/Fevin_species-list.csv")
spp_devin %<>% pull(species)


## FENOMOR SPECIES
spp_fenomor <- 
  read_csv("./data/compostion/fenomor-spp-list.csv") %>%
  pull(Species_list) %>%
  unique %>%
  sort

## Species from Pladias

spp_plad <- read_csv("data/traits/traits_transformed_standardized_missing.replaced.by.mean.csv")
spp_plad %<>% .$X1
spp_plad %<>% str_replace_all("_", " ")

#save.image("lists-of-spp.RData")
