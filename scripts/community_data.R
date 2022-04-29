source("Source_libraries.R")

### CREATE A NEW DATABASE WITH COMMUNITIES AND COMMUNITY AGGREGATED TRAITS

## Community x Traits table

# Let's put the names of the communities in rows:
comm_data <-
  pre_data_gl[1, ] %>% t 

# Now let's add the columns, preliminarily with FALSE values, of traits

precomm_mat <- matrix(rep(vector("logical", length = nrow(comm_data)), 
                          length(funct_data_total)), 
                      ncol = length(funct_data_total))

{
  rownames(comm_data) <- NULL; 
  comm_data <- cbind(comm_data, precomm_mat) %>% as_data_frame
}

comm_data
colnames(comm_data) <- c("Rel_number", colnames(funct_data_total))
comm_data <-  comm_data[-c(1, 2), ]

## Now let's create a "new pladias" with a trait value, not for the species, but for 
#  the communities

## Why does this pre_data
