
#### VISUALIZATION LEAF WEIGHT 

trait_leaves <- read_csv("../data/traits_leaves_devin.csv")
trait_leaves %<>%
  mutate(
    `fresh_weight(mg)` = `fresh_weight(mg)`/10,
      `dry_weight(mg)` = {
        str_replace_all(`dry_weight(mg)`, ",", ".") %>%
        as.numeric()
    }
  ) %>%
  arrange(species)
## However, from the last plot, we know there is a misreading
problematic_vals <- which(trait_leaves$`dry_weight(mg)` >= trait_leaves$`fresh_weight(mg)`)
trait_leaves[problematic_vals, ]

## It's a problem coming from the reading of the file. All integers were kept equal
#  while the rest were multiplied by 10, hence the error, since dividing everything
#  by 10 also divided the correct integer numbers. Let's fix it

fixed_vals <- trait_leaves %>%
  pull(`fresh_weight(mg)`) %>%
  .[problematic_vals] * 10

trait_leaves[problematic_vals, "fresh_weight(mg)"] <- fixed_vals
trait_leaves[problematic_vals, ]


#### NORMAL GGPLOT WITH MAP FUNCTIONS ####

### PLOTS FRESH WEIGHT
## Let's define what will be an argument for our plotting function:
spp_names <-
  trait_leaves %>%
  pull(species) %>%
  unique()

plot_histogram_fw <- function(filt){
  trait_leaves %>%
    filter(species == {{filt}}) %>%
    ggplot(aes(x = `fresh_weight(mg)`)) +
    geom_histogram() +
    labs(title = str_glue("Histogram for fresh weight of {filt}")) +
    theme_bw()
}

histos_fw <-
  spp_names %>%
  map(plot_histogram_fw)

histos_fw %>%
  walk(print)

### PLOTS DRY WEIGHT
plot_histogram_dw <- function(filt){
  trait_leaves %>%
    filter(species == {{filt}}) %>%
    ggplot(aes(x = `dry_weight(mg)`)) +
    geom_histogram() +
    labs(title = str_glue("Histogram for dry weight of {filt}")) +
    theme_bw()
}

histos_dw <-
  spp_names %>%
  map(plot_histogram_dw)

histos_dw %>%
  walk(print)

#### NORMAL GGPLOT WITH FACET_WRAP FUNCTION ####
trait_leaves %>%
  ggplot(aes(x = `fresh_weight(mg)`)) +
  geom_histogram(binwidth = 5) +
  theme_bw() +
  facet_wrap( ~ species) +
  labs(title = "Fresh weight (mg)",
       caption = "Samples obtained on 06/04/2022")

trait_leaves %>%
  ggplot(aes(x = `dry_weight(mg)`)) +
  geom_histogram(binwidth = .5) +
  theme_bw() +
  facet_wrap( ~ species) +
  labs(title = "Dry weight (mg)",
       caption = "Samples obtained on 06/04/2022")


#### GGSTATSPLOT #####
library(ggstatsplot)

gghistostats()
ggwithinstats()
ggbetweenstats()

#### FRESH WEIGHT
gghistos_fw <- function(filt){
  trait_leaves %>%
    filter(species == {{filt}}) %>%
    gghistostats(
      x = `fresh_weight(mg)`,
      normal.curve = TRUE) +
    labs(title = str_glue("Fresh weight (mg) of {filt}"),
         caption = "Samples obtained on 06/04/2022")
}


gghistoplots_fw <-
  spp_names %>%
  map(gghistos_fw)

gghistoplots_fw %>% walk(print)

for(i in 1:length(gghistoplots_fw)){
  ggsave(filename = paste0("../../PhD/hist_fw_", str_replace(spp_names[i], " ", "_"), ".png"), plot = gghistoplots_fw[[i]])
}

grouped_gghistostats(
  data = trait_leaves,
  x = `fresh_weight(mg)`,
  grouping.var = species, 
  normal.curve = TRUE
)
ggsave(filename = "../../PhD/grouped_hist_fw.png")

#### DRY WEIGHT
gghistos_dw <- function(filt){
  trait_leaves %>%
    filter(species == {{filt}}) %>%
    gghistostats(
      x = `dry_weight(mg)`,
      normal.curve = TRUE) +
    labs(title = str_glue("Dry weight (mg) of {filt}"),
         caption = "Samples obtained on 06/04/2022")
}

gghistoplots_dw <-
  spp_names %>%
  map(gghistos_dw)

gghistoplots_dw %>% walk(print)

for(i in 1:length(gghistoplots_dw)){
  ggsave(filename = paste0("../../PhD/hist_dw_", str_replace(spp_names[i], " ", "_"), ".png"), plot = gghistoplots_dw[[i]])
}

grouped_gghistostats(
  data = trait_leaves,
  x = `dry_weight(mg)`,
  grouping.var = species, 
  normal.curve = TRUE
)
ggsave(filename = "../../PhD/grouped_hist_dw.png")

#### COMPARISON BETWEEN FRESH AND DRY WEIGHT ####

grouped_ggwithinstats(
  data             = trait_leaves %>%
                      pivot_longer(c(`fresh_weight(mg)`, `dry_weight(mg)`),
                      names_to = "type_weight"),
  x                = type_weight,
  y                = value,
  type             = "", ## non-parametric statistics
  xlab             = "Type of weight",
  ylab             = "Weight (mg)",
  grouping.var     = species,
  outlier.tagging  = TRUE,
  pairwise.display = TRUE,
  point.path       = TRUE
) + labs(caption = "Samples taken at Devin, Pavlov Hills, on 06/04/2022")

## Now we see something interesting: some individuals (between 2 and 4) in each sp
#  have equal or higher dry weight than fresh weight.

# problematic_vals <- which(trait_leaves$`dry_weight(mg)` >= trait_leaves$`fresh_weight(mg)`)
# 
# trait_leaves[problematic_vals, ]

## It's a problem coming from the reading of the file. All integers were kept equal
#  while the rest were multiplied by 10, hence the error, since dividing everything
#  by 10 also divided the correct integer numbers. Let's fix it

# fixed_vals <- trait_leaves %>%
#   pull(`fresh_weight(mg)`) %>%
#   .[problematic_vals] * 10

# trait_leaves[problematic_vals, "fresh_weight(mg)"] <- fixed_vals
# trait_leaves[problematic_vals, ]

## Now run again the plots.
## NOT NEEDED. Corrections done at the beginning. 