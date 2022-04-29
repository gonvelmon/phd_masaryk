source("./Gonzalo R/PhD/Source_libraries.R")

# Data cleaning -----------------------------------------------------------

getwd()
readr::locale()

##### Phytosociological data ####
codes_cz <- read_tsv("../Q4 2021/data/CZ data codes.tsv", col_names = F)
?read_delim
pre_data_gl <- read_delim("Gonzalo R/PhD/data/composition/Excl_exp_Festuco-Brometea2.txt", delim = ";",
                       col_names = F)
read_delim("./data/dataspxp.txt", delim = "\t0")
View(read_tsv("./data/dataspxp.txt"))



pre_data_gl %>% View()

## Now let's create a list-column data frame, gathering the data on all the species 
#  by community in a list-entry.
phyto_data <- pre_data_gl 

phyto_data %<>% t()
phyto_data %>% print() %>% View
phyto_data %<>% .[-2, ]
colnames(phyto_data)  <- phyto_data[1, ] 
phyto_data %<>% .[-1,]
phyto_data %>% View()
phyto_data %>% summary()
phyto_data %<>% as_tibble()
colnames(phyto_data[, 1:40])
phyto_data[, -c(2:34)] %>% View

ggplot(data = phyto_data[, -c(2:34)]) +
  geom_histogram(mapping = aes(x = as.numeric("Acinos arvensis")))
  #geom_histogram(mapping = aes(x = "Agrostis capillaris"))

phyto_data_spp <- phyto_data[, -c(2:34)]

apply(apply(phyto_data_spp[, -1], 2, as.numeric), 2, rank) %>% sort(decreasing = T)
phyto_data1 <- phyto_data %>%
  group_by(.[, 1:34]) %>%
  nest()

phyto_data1 %<>% .[,-1]
phyto_data1 %>% View()


