source("Source_libraries.R")
source("Data_Loading.R")

#### Check Species with no presence in Pladias

spp_list <- read_csv("./data/Fevin_species-list.csv")
spp_list %<>% select(species)

spp_plad <- read_csv("../Q4 2021/data/traits_transformed_standardized_missing.replaced.by.mean.csv")
spp_plad %<>% select(X1)
spp_plad

spp_plad_ws <-
  spp_plad %>%
  apply(1, str_replace_all, "_", " ") %>%
  as_tibble()
names(spp_plad_ws) <- "species"

spp_plad_ws$species %in% spp_list$species %>% sum # 77 species of 84 are in Pladias
shared_spp <- intersect(spp_list$species, spp_plad_ws$species)
shared_spp

complementary_spp <- spp_list[!is.element(spp_list$species, shared_spp), ]
complementary_spp # 2 entries are families, will have to remove them

complementary_spp_simp <-
  complementary_spp %>% 
  as_vector %>%
  str_trim %>%
  as_tibble %>%
  separate(col = 1, into = c("Genus", "species"), sep = "_") %>%
  filter(!str_detect(.$Genus, "ceae$")) # To remove families Boraginaceae and Brassicaceae

complementary_spp_simp


any(spp_plad_ws$species == "Alyssum montanum") # It is
any(spp_plad_ws$species == "Viola tricolor") # It is

## So, the problem with these 2 species is that in Devin they are identified with the subspecies name:
#  Alyssum montanum subsp. gmelinii
#  Viola tricolor subsp. saxatilis
## Besides, there are 3 unidentified spp from the genera Bromus, Cerastium and Veronica

affine_fucnt_spp <- spp_plad_ws[str_detect(spp_plad_ws$species, complementary_spp_simp$Genus[1:3]), ] 
affine_fucnt_spp
