
source("Source_libraries.R")
source("./scripts/DataLoading.R")

##### GLOBAL LIST OF TRAITS WITH OLD PLADIAS DATA ####

names(funct_data_pladias)

obsidian_traits <- c("[Ll]eaf", "[Gg]rowth", "[Ll]ife", "[Ff]orm", "[Ff]lowering", "[Rr]eproduction", "Dispersal",
                     "[I]nvasion", "[Ff]ruit")

sp_name <- 
  c(spp_fen_corr, spp_devin$species) %>%
  unique %>%
  sort

x <- list()
for(i in 1:length(obsidian_traits)){
  x[[i]] <- colnames(funct_data_pladias)[grep(obsidian_traits[i], colnames(funct_data_pladias))]
}

obs_info <- function(sp_name){
  funct_data_pladias[c(1, 2, which(funct_data_pladias$species_name == sp_name)), 
             unlist(x)]
}

obs_list <- lapply(sp_name, obs_info)

names(obs_list) <- sp_name

test_name <- "Minuartia rubra"
obs_list %>%
  .[[test_name]] %>%
  colnames %>%
  grep("[Ll]eaf", .) %>%
  obs_list[[test_name]][.]

##### 1 ) LEAF TRAITS ####

lapply(obs_list, "[", obs_list %>%
                        .[[test_name]] %>%
                        colnames %>%
                        grep("[Ll]eaf", .)
)$`Achillea pannonica` %>% View
funct_data_pladias[funct_data_pladias$species_name == "Achillea collina", ]

new_pladias_na[new_pladias_na$species == "Achillea collina", ]

##### 2 ) GROWTH FORM AND LIFE TRAITS ####


lapply(obs_list, "[", obs_list %>%
                        .[[test_name]] %>%
                        colnames %>%
                        grep("[Gg]rowth", .)
)$`Viola tricolor`

lapply(obs_list, "[", obs_list %>%
                        .[[test_name]] %>%
                        colnames %>%
                        grep("[Ll]ife", .)
)$`Viola tricolor`

##### 3 ) FLOWERING TRAITS ####

lapply(obs_list, "[", obs_list %>%
                        .[[test_name]] %>%
                        colnames %>%
                        grep("[Ff]lower", .)
)$``

#### 4 ) DISPERSAL TRAITS ####

lapply(obs_list, "[", obs_list %>%
                        .[[test_name]] %>%
                        colnames %>%
                        grep("[Dd]ispersal", .)
)$``

#### 5 ) REPRODUCTIVE TRAITS ####

lapply(obs_list, "[", obs_list %>%
                        .[[test_name]] %>%
                        colnames %>%
                        grep("[Rr]eproduct", .)
)$``

#### 6 ) INVASIVE ####

lapply(obs_list, "[", obs_list %>%
                        .[[test_name]] %>%
                        colnames %>%
                        grep("Invasion", .)
)$``





