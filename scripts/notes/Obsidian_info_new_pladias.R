
source("scripts/DataLoading.R")

#### LIST OF TRAITS WITH NEW PLADIAS DATA ####

names(new_pladias_na)

lapply(new_pladias_na, is_double) ## This identifies the type of the variable
# ATTENTION. It doesn't work with normal apply

## Let's see the strictly numerical variables:
new_pladias_na %>%
  select(which(sapply(., is.double))) # Only "species" is not double (from 71 to 70 cols)

new_pladias_na %>%
  apply(2, str_detect, "[:alpha:]") # Only "species" is filled with alphabetical values


## Now let's select the boolean or quasiboolean variables

new_pladias_na_bool_test <-
  new_pladias_na %>%
  sapply(str_detect, "[^0-1]") %>%
  not %>%
  as_tibble() %>%
  apply(2, sum, na.rm = T) %>%
  as.data.frame() %>%
  rownames_to_column(var = "bool_var_names") %>%
  rename(
    sum_boolean = "."
  ) %>% 
  filter(
    sum_boolean >= 1555 # Threshold. Consider Boolean and quasiboolean variables that have >66% of values 0 OR 1
     )
new_pladias_na_bool_test
bool_var_names <- new_pladias_na_bool_test$bool_var_names


## Now let's see which spp are not boolean, and by how much
new_pladias_na_quan_test <-
  new_pladias_na %>%
  sapply(str_detect, "[^0-1]") %>%
  not %>%
  as_tibble() %>%
  apply(2, sum, na.rm = T) %>%
  as.data.frame() %>%
  rownames_to_column(var = "var_name_quan") %>%
  rename(
    sum_boolean = "."
  ) %>% 
  filter(
    sum_boolean < 1555
  )
new_pladias_na_quan_test

new_pladias_na_bool <- 
  new_pladias_na %>%
  select(
    species,
    which(colnames(new_pladias_na) %in% bool_var_names)
    )
new_pladias_na_bool %>% View()
## At least Tree, Shrub, Dioecy, Parasit and MHtroph are quasiboolean

new_pladias_na_quan <-
  new_pladias_na %>%
  select(
    species,
    !which(colnames(new_pladias_na) %in% bool_var_names)
  )
new_pladias_na_quan

## There are 19 non-boolean variables:
#  X1, species, Height, LA, SLA, LDMC, SeedMass, Gnm_size, Gnm_GC,
#  FloweringLength, OutCross (quarters form 0 to 1), VegetRepr (qrts 0:1), BdBank_size, BdBank_depth,
#  Clon_persist, Clon_multip, LateralSpr, ClonInd 
#  (all of the last line difficult to interpret)

## However:
#  new_pladias_na_quan_test
#  We see very low values, but not 0s, for variables which we know are non-boolean.
#  These are SLA, LDMC and SeedMass

## Culprit SLA (1)
which(
  near(new_pladias_na$SLA, 0) | near(new_pladias_na$SLA, 1)
  )

# DOES NOT FIND IT

#apply(new_pladias_na, 2, grepl, "[^0-1]")

## Culprits LDMC (4)
which(
  near(new_pladias_na$LDMC, 0) | near(new_pladias_na$LDMC, 1)
  )
# DOES NOT FIND THEM

## Culprits Seed Mass (4)
which(new_pladias_na$SeedMass == 0 | new_pladias_na$SeedMass == 1)
new_pladias_na[c(439, 1531, 1833, 2189), c("species", "SeedMass")] 

# All the following have values of 0:
# Carex secalina, Physocarpus opulifolius (!), Rubus odoratus (!), Trifolium montanum (!)


#### GET THE INFO FOR OBSIDIAN ####

#Need to run:
  source("./scripts/spp_fenomor.R")

sp_name <- 
  c(spp_fen_corr, spp_devin$species) %>%
  unique %>%
  sort

#### First let's get the columns of boolean variables

x_new_bool <- colnames(new_pladias_na_bool) 
## This assignation solves the problem fixed by lines 153-162

# for(i in 1:length(sp_name)){
#   x_new_bool[[i]] <- colnames(new_pladias_na_bool)
# }

obs_info_new_bool <- function(sp_name){
  new_pladias_na_bool[which(new_pladias_na_bool$species == sp_name), 
                     x_new_bool]
}

obs_list_new_bool <- lapply(sp_name, obs_info_new_bool)

names(obs_list_new_bool) <- sp_name[-161]

## Finally, remove empty dataframe entries
obs_list_new_bool %<>%
  .[
  sapply(
    obs_list_new_bool, nrow  # Take advantage of having just 1 (or 0) rows in each dataframe
    ) == T                   # so it's immediately translatable to logical values
  ]

#obs_list_new_bool

#### Now let's see the quantitative variables:

x_new_quan <- colnames(new_pladias_na_quan)

obs_info_new_quan <- function(sp_name){
  new_pladias_na_quan[which(new_pladias_na_quan$species == sp_name),
                      x_new_quan]
}

obs_list_new_quan <- lapply(sp_name, obs_info_new_quan)

names(obs_list_new_quan) <- sp_name[-161]

obs_list_new_quan %<>%
  .[
    sapply(
      obs_list_new_quan, nrow
    ) == T
  ]

## Chunk to remove duplicate columns from a dataframe

# test_name <- "Minuartia rubra"   # "Excuse" to enter in any of the elements of the list
# obs_list_new_quan <-
#   lapply(
#     obs_list_new_quan,
#     subset,
#     select = which(!duplicated(colnames(obs_list_new_quan[[test_name]]
#                                         )
#                               )
#                   )
#         )


##### 1 ) BOOLEAN TRAITS ####
names(obs_list_new_bool)

obs_list_new_bool[[
                   "Anthericum ramosum" # Inexplicably, for some spp, like the first one
                 ]] %>% select_if(. > 0) %>% View             # the select does not work
  

##### 2 ) QUANTITATIVE TRAITS ####
obs_list_new_quan[[
                   "Alyssum montanum"
                 ]] %>% 
  View




#### 3 ) WEB SCRAPING TRIALS

library(rvest)
url_pladias <- "https://pladias.cz/en/taxon/data/"
url_pladias_test <- paste0(url_pladias, 
                           sp_name %>%
                             .[!str_detect(., "\\.")] %>%
                             str_replace_all(" ", "%20")
                           )
sp_name[!str_detect(sp_name, "\\.")]
# pages <- read_html(url_pladias_test[1:5]) # Doesn't work in vectorized way

#sp_name %>%
#  as.symbol()

#web_data[[as.symbol(sp_name[1])]] <- "A. collina"

web_data <- list()
  for(i in 1:length(names(obs_list_new_bool))){
      web_data[[as.symbol(names(obs_list_new_bool)[[i]])]] <-
          url_pladias_test[i] %>%
          read_html() %>%
          html_elements(xpath = "//*[@class = 'ml-4']") %>%
          html_text2() %>%
          str_split("\n")
    }


data_retriever <- function(){
  url_pladias_test[] %>%
    read_html() %>%
    html_elements(xpath = "//*[@class = 'ml-4']") %>%
    html_text2() %>%
    str_split("\n")
}

apply(names(obs_list_new_bool), data_retriever, )


web_data$`Achillea collina`
web_data$`Viola rupestris`


sapply(web_data, length) %>%
  max()

which(sapply(web_data, length) == 23) 
# Arrhenatherum elatius, Medicago lupulina, Melica ciliata, Valerianella locusta

web_data$`Medicago lupulina`

lapply(web_data, str_remove_all, "")

lapply(web_data, "[")

sapply(web_data, length) %>%
  .[[str_detect(., "[:digit:]")]]


web_data_test <- vector(mode = "list", length = length(web_data))
names(web_data_test) <- names(web_data)

for(i in 1:length(web_data)){
  web_data_test[i] <- 
    web_data %>%
    .[[
    i
    ]][
        lapply(web_data[[1]], str_detect, "") %>%
        unlist
      ]
  }

web_data_test

web_data[[1]]

lapply(web_data,
          lapply(web_data[1], str_detect, "") %>%
          unlist)

## Problem: the list has an extra layer of storage

web_data$`Achillea collina`[[1]][lapply(web_data[[1]], str_detect, "") %>%
                unlist]

unlist(web_data)

is_null(web_data[["Achillea collina"]])

web_data$`Achillea collina`[207]

lapply(web_data, list) %>%
  unlist %>%
  vapply(., is_null, FALSE) 

