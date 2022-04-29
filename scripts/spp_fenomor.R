pacman::p_load(tidyverse, magrittr, naniar)
source("scripts/DataLoading.R")
load("lists-of-spp.RData")

#### FENOMOR SPECIES LIST #####


### We have 156 spp. in the "Fenomor" communities

new_pladias_na$species %in% {
  spp_fenomor %>%
    unique %>%
    sort
  } %>% 
  new_pladias_na$species[.] %T>%
  print %>%
  length

## We have 122 species from Fenomor which are recorded in Pladias.
#  First see what spp are in Pladias

spp_fen_1_plad <-
  spp_fenomor %in% {
    new_pladias_na$species %>%
      unique %>%
      sort
    } %>%
  spp_fenomor[.] %>%
  unique %>%
  sort

#spp_fen_1_plad

## And now what spp are NOT there:

spp_fen_0_plad <-
  spp_fenomor %>%
  unique %>%
  subset(
         !(. %in% spp_fen_1_plad)
         )

#spp_fen_0_plad

## 34 entries which are not in Pladias, BUT:
#  Many of them have some special character (like "!") or are unintelligible

## Also note the misspellings (see next section):
#  Sang[u]isorba minor
#  Asperula cyny[a]chica


##### COMPARING DEVIN AND FENOMOR COMMUNITIES #####

## 1) Shared species

spp_dev_fen <- 
  spp_fenomor %in% spp_devin %>%
  spp_fenomor[.] %>%
  unique %>%
  sort

spp_devin %in% spp_fenomor %>%
  spp_devin[.] %>%
  unique %>%
  sort

#spp_dev_fen # 55 entries shared between the 2 datasets


## 2) Species exclusive from Fenomor 

spp_fen_exc <-
  spp_fenomor %>%
    .[
      which(
        !spp_fenomor %in% spp_dev_fen)
      ] %>%
    unique %>%
    sort

#spp_fen_exc # 101 spp exclusive from Fenomor

## 3) Species exclusive from Devin

spp_dev_exc <- 
  spp_devin[
  which(
    !spp_devin %in% spp_fenomor)
  ]

spp_dev_exc

## Now let's analyze the spp from Fenomor:

spp_fen_exc

# It has around 100 spp which are not in Devin.
# Alyssum montanum is in Devin but in the ssp. gmelinii

## Careful! Jovibarba globifera is in Devin and here appears 2 times as 
#  "Jovibarba glob. ssp. hirta" and "Jovibarba gl. ssp. hirta !!!" 

#### Difficult entries in the Fenomor tables:

## Abbreviations, unidentified, special symbols or unintelligible 
# 1 ) Muscari sp.
# 2 ) Festuca cf. valesiaca (Devin has Festuca valesiaca)
# 3 ) Erysimum sp.
# 4 ) Cerastium semidecand. (Cerastium semidecandrum)
# 5 ) Viola tricolor cf. (Devin has Viola tricolor)
# 6 ) Chamaecytisus ratisbon. (Chamaecytisus ratisbonensis)
# 7 ) cf. Cirsium
# 8 ) Thymus sp.
# 9 ) trava   Table 7-1
# 10 ) cf. Arrhenatherum elatius (Devin has Arrhenatherum elatius)
# 11 ) semenacek (5 times in the list)
# 12 ) Achillea sp.
# 13 ) Galatella linosyris, pravy dolni roh ("right lower corner")
# 14 ) Sesleria caerulea!  Table 8-5
# 15 ) Vincetoxicum hirundiar. (Devin has Vincetoxicum hirundinaria)
# 16 ) Allium sp.
# 17 ) Stipa sp.
# 18 ) Erysimum sp.
# 19 ) Galium sp.
# 20 ) Dianthus lumnitzeri !!! Table 5-1
# 21 ) cf. Melica ciliata
# 22 ) Galium sp.
# 23 ) Festuca

## Subspecies
# 24 ) Jovibarba glob. ssp. hirta
# 25 ) Jovibarba gl. ssp. hirta !!!"  Table 2-6
# 26 ) Allium senes. ssp. mont.
# 27 ) Valeriana stol. ssp. angus.
# 28 ) Orobanche alba ssp. alba

## Aggregates
# 29 ) Thymus pannonicus agg.

## Wrong names:
# 30) Asperula cynynchica (it's Asperula cynanchica). Table 2-1
# 31) Sangisorba minor    (it's Sanguisorba minor).   Table 8-2


#### COMPARE MISSINGNESS IN PLADIAS FOR THE 2 COMMUNITIES ####

#### (Apply only once the names of spp_fen_0_pladias have been corrected)

spp_fen_0_plad_corr <- c("Festuca valesiaca", "Jovibarba globifera", 
                         "Allium senescens", "Melica ciliata",
                         "Asperula cynanchica", "Vincetoxicum hirundinaria",
                         "Cerastium semidecandrum", "Dianthus lumnitzeri",
                         "Viola tricolor", "Valeriana stolonifera",
                         "Thymus pannonicus", "Chamaecytisus ratisbonensis",
                         "Galatella linosyris", "Arrhenatherum elatius",
                         "Orobanche alba", "Nigella arvensis")


spp_fen_corr <- 
  c(spp_fen_1_plad, spp_fen_0_plad_corr) %>%
  unique %>%
  sort

#spp_fen_corr %>% intersect(new_pladias_na$species)  # 130 spp are in New Pladias

spp_dev_1_plad <-
  read_csv("./data/devin_spp_nas_lqt-seedmass.csv") %>%
    filter(LQTs_nas > 0 | Seed_Mass_na > 0) %>%
    pull(species) %>%
    str_replace_all("_", " ")

spp_fen.dev_1_plad <-
  dplyr::intersect(
    spp_dev_1_plad, spp_fen_1_plad
    )
#spp_fen.dev_1_plad # (at least) 20 spp shared that lack some of the data

spp_dev_0_plad <-
  read_csv("./data/devin_spp_nas_lqt-seedmass.csv") %>%
  filter(LQTs_nas == 0 & Seed_Mass_na == 0) %>%
  pull(species) %>%
  str_replace_all("_", " ")
spp_dev_0_plad

spp_fen.dev_0_plad <-
  dplyr::intersect(
    spp_dev_0_plad, spp_fen_0_plad
  )
#spp_fen.dev_0_plad ## No shared spp lacking all types of data (seed mass and LQTS)

##### NAs IN PLADIAS FOR THE SPP IN FENOMOR ####

ggpubr::ggarrange(
  new_pladias_na %>%
    .[new_pladias_na$species %in% sort(spp_fen_corr, decreasing = T), ] %>%
    apply(., 2, naniar::all_complete) %>%
    not %>%
    new_pladias_na[, .] %>% 
    .[new_pladias_na$species %in% spp_fen_corr[1:(length(spp_fen_corr)/2)], ] %>%
    naniar::vis_miss(sort_miss = TRUE) + 
    labs(
      title = "NAs for species in Fenomor (first half)",
      y = "Species identity"
         ) +
    scale_y_discrete(
      limits = spp_fen_corr[1:(length(spp_fen_corr)/2)]
         ) +
    theme(plot.title = element_text(face="bold")
          )
  ,
  new_pladias_na %>%
    .[new_pladias_na$species %in% sort(spp_fen_corr, decreasing = T), ] %>%
    apply(., 2, naniar::all_complete) %>%
    not %>%
    new_pladias_na[, .] %>% 
    .[new_pladias_na$species %in% 
        spp_fen_corr[
          (length(spp_fen_corr)/2+1):length(spp_fen_corr)
        ], 
      , ] %>%
    naniar::vis_miss(sort_miss = TRUE) + 
    labs(
      title = "NAs for species in Fenomor (second half)",
      y = ""
    ) +
    scale_y_discrete(
      limits = spp_fen_corr[(length(spp_fen_corr)/2+1):length(spp_fen_corr)]
    ) +
    theme(plot.title = element_text(face="bold")
          )
  )

#### Species from Fenomor lacking some traits

which(!spp_fen_corr %in% new_pladias_na$species)
## Nigella arvensis needs to be added to Pladias


spp_fen_LQT_nas <- 
  which(new_pladias_na$species %in% spp_fen_corr) %>%
  new_pladias_na[., ] %>%
  #rowwise() %>%
  .[
    is.na(.$SLA) | is.na(.$LA) | is.na(.$LDMC)
    , 
    ] %>%
  pull(species)

spp_fen_SM_nas <-
  which(new_pladias_na$species %in% spp_fen_corr) %>%
  new_pladias_na[., ] %>%
  #rowwise() %>%
  .[
    is.na(.$SeedMass)
    , 
  ] %>%
  pull(species)

spp_fen_nas <-
  c(spp_fen_LQT_nas, spp_fen_SM_nas) %>%
  unique() %>%
  sort()

spp_total_nas <-
  c(spp_fen_nas, spp_dev_1_plad) %>%
  unique %>%
  sort


tibble("species_nas_all" = spp_total_nas,
       "species_fenomor_nas" = c(spp_fen_nas, vector(length = length(spp_total_nas) - length(spp_fen_nas))), 
       "species_fenomor_LQT_nas" = c(spp_fen_LQT_nas, vector(length = length(spp_total_nas) - length(spp_fen_LQT_nas))),
       "species_fenomor_SM_nas" = c(spp_fen_SM_nas, vector(length = length(spp_total_nas) - length(spp_fen_SM_nas))), 
       "species_devin_nas" = c(sort(spp_dev_1_plad), vector(length = length(spp_total_nas) - length(spp_dev_1_plad)))
      ) %T>% View %>% 
  write_csv("species_nas_fenomor_and_devin.csv")

