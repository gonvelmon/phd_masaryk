
source("Source_libraries.R")
source("Data_loading.R")

#### Check Species with no presence in Pladias

spp_list %<>%
  apply(1, str_replace_all, " ", "_") %>%
  as_tibble()
names(spp_list) <- "species"

spp_plad$species %in% spp_list$species %>% sum # 77 species of 84 are in Pladias
shared_spp <- intersect(spp_list$species, spp_plad$species)
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

# Are any of the species identified to subspecies level present in Pladias?
any(spp_plad$species == "Alyssum_montanum") # It is
any(spp_plad$species == "Viola_tricolor") # It is

## So, the problem with these 2 species is that in Devin they are identified with the subspecies name:
#  Alyssum montanum subsp. gmelinii
#  Viola tricolor subsp. saxatilis
## Besides, there are 3 unidentified spp from the genera Bromus, Cerastium and Veronica

affine_fucnt_spp <- spp_plad[str_detect(spp_plad$species, complementary_spp_simp$Genus[1:3]), ] 
affine_fucnt_spp
# For the non_whitespaces version:
# affine_fucnt_spp %>% apply(1, str_replace_all, "_", " ")
