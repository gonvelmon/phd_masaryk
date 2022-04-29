pacman::p_load(tidyverse, magrittr, patchwork)
#### Information on species present in Devin
source("../Devin/scripts/species_comparison_nows.R")
source("scripts/DataLoading.R")
## Check that the data from the script have been created
#shared_spp
#complementary_spp
#complementary_spp_simp
#affine_funct_spp

naniar::vis_miss(new_pladias_na)

new_pladias_na %>%
  filter(.$species %in% shared_spp) %>%
  naniar::vis_miss(sort_miss = TRUE) + labs(title = "NAs for species in Devin",
                                            y = "Species identity")

new_pladias_na %>%
  filter(.$species %in% affine_funct_spp$species) %>%
  naniar::vis_miss(sort_miss = TRUE) + labs(title = "NAs for phylogenetically affine species to the 
                            Bromus sp., Cerastium sp. and Veronica sp. in Devin",
                                            y = "Species identity")
                                         

?naniar::vis_miss
?vis_miss
?rank


#### NA Analysis for Leaf Quantitative Traits (LQTs) ####

sort_dev_spp_byna <-
  new_pladias_na %>%
  filter(.$species %in% shared_spp) %>%
  select(LA, SLA, LDMC) %>%
  apply(., 2, is.na) %>%
  as_tibble %>%
  add_column(
    sum_nas = apply(., 1, sum)
  ) %>%
  add_column(., species = sort(shared_spp), .before = 1) %>%
  arrange(desc(sum_nas))

write_csv(sort_dev_spp_byna, file = "Devin_spp_bynas_leaves.csv")


dev_spp <- read_csv("../Devin/data/Fevin_species-list.csv")
dev_spp
dev_spp_nas_leaves <-  
  dev_spp %>%
  apply(2, str_replace_all," ", "_")  %>%
  merge(., sort_dev_spp_byna, by = "species") %>%
  as_tibble %>%
  arrange(desc(n)) %>%
  mutate(
    n = as.numeric(str_remove_all(n, "_"))
  )

p_dev_nas_leaves <-
  dev_spp_nas_leaves %>%
  ggplot(., aes(x = n, y = sum_nas, color = n, shape = history)) +
  geom_point(size = 2) +
  geom_hline(yintercept = c(0, 1, 2, 3), lty = 2) +
  xlim(c(-1, 750)) +
  ylim(c(-0.2, 3.5)) +
  geom_text(aes(label = species %>% str_replace("_", " ")), nudge_y = 0.15, 
            nudge_x = 8,angle = 38, check_overlap = TRUE,
            fontface = "italic", alpha = .5) +
  labs(x = "Abundance", y = "Sum of NAs",
       title = "Sum of NAs for LA, SLA and LDMC in Devin species",
       caption = "We should focus on taking measurments for species on the top line, from right to left",
       color = "Abundance",
       shape = "Life-History:") +
  #guides(fill = guide_legend(title = "Title")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_text(face = "bold"),
        legend.position = "bottom") +
  scale_color_gradient(low="darkgreen", high="red")

p_dev_nas_leaves + guides(color = FALSE) + scale_x_reverse()

#nas_devin_spp %>%
 # apply(2, na_if, TRUE) %>%
 # as_tibble %>%
 # naniar::vis_miss()



#### NA Analysis of Seed Mass ####
dev_spp_nas_seeds <-
  new_pladias_na %>%
  filter(.$species %in% shared_spp) %>%
  select(SeedMass) %>%
  add_column(
    IsNA = apply(., 2, is.na)
    ) %>%
  as_tibble %>%
  add_column(
    ., species = sort(shared_spp), .before = 1
  ) %>%
  arrange(desc(IsNA)) %>%
  apply(
    2,
    str_replace, "_", " "
  ) %>%
  merge(., y = arrange(dev_spp, desc(species)), by = "species") %>%
  mutate(
    IsNA = as.numeric(as.logical((IsNA)))
  )



p_dev_nas_seeds <-
  dev_spp_nas_seeds %>%  
  ggplot(., aes(x = n, y = IsNA, color = n, shape = history)) +
  geom_point(size = 2) +
  geom_hline(yintercept = c(0, 1), lty = 2) +
  geom_text(aes(label = species %>% str_replace("_", " ")), 
            nudge_x = 8, nudge_y = 0.07, angle = 38, check_overlap = TRUE,
            fontface = "italic", alpha = .5) +
  labs(x = "Abundance", y = "NA for Seed Mass",
       title = "NAs for Seed Mass in Devin species",
       caption = "We should take measurments for species on the top line, from right to left",
       color = "Abundance",
       shape = "Life-History:") +
  #guides(fill = guide_legend(title = "Title")) +
  xlim(c(-10, 750)) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_text(face = "bold"),
        legend.position = "bottom") +
  scale_color_gradient(low="darkgreen", high="red") +
  scale_y_continuous(breaks = c(0, 1),
                   labels = c("0", "1")
                   ) +
  ylim(c(-0.2, 1.3))

p_dev_nas_seeds + guides(color = "none")

#### MERGE THE DATA ON NAs

dev_spp_nas <- full_join(
  dev_spp_nas_leaves, 
  dev_spp_nas_seeds %>%
    apply(2, str_replace_all, " ", "_"),
  by = "species", copy = TRUE
  )

dev_spp_nas %<>%
  select(-c(n.y, area.y, history.y))
dev_spp_nas %<>%
  rename(
    LQTs_nas = sum_nas,
    Seed_Mass_na = IsNA
    )

dev_spp_nas %<>%
  arrange(species) %>%
  select(species, LQTs_nas, Seed_Mass_na, SeedMass, everything(), -LA, -SLA, -LDMC) %>%
  add_column(new_pladias_na %>%
               filter(.$species %in% shared_spp) %>%
               select(LA, SLA, LDMC)) %>%
  arrange(desc(LQTs_nas), desc(Seed_Mass_na))
dev_spp_nas$Seed_Mass_na <- as.integer(dev_spp_nas$Seed_Mass_na)
dev_spp_nas[77, 3] <- 1
dev_spp_nas %<>%
  arrange(desc(LQTs_nas), desc(Seed_Mass_na))

#View(dev_spp_nas)

write_csv(dev_spp_nas, "./data/devin_spp_nas_lqt-seedmass.csv")
