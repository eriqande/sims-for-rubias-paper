
library(tidyverse)
library(forcats)
library(rubias)

# get the simulation outputs:
load("data/cjfas_simulation_results.Rdata")


# Hass_rho_dev is hasselmen data done using Leave-One-Out and summarized
# mc_rho_dev is hasselman data done by monte carlo cross validation
# coal_rho_dev is coalescent-simulated data summarized

# here we clean them all up

# new variable names and get rid of the factors
LOO <- Hass_rho_dev %>%
  mutate(repunit = as.character(repunit),
         method = as.character(method))
MCCV <- mc_rho_dev %>%
  mutate(repunit = as.character(repunit),
         method = as.character(method))
COAL <- coal_rho_dev %>%
  mutate(repunit = as.character(repunit),
         method = as.character(method))



# get the number of collections in each reporting unit in 
# the alewife data
alewife_Krs <- alewife %>%
  count(repunit, collection) %>%
  select(-n) %>%
  group_by(repunit) %>%
  tally() %>%
  rename(Kr = n)



# also, record the number of collections in each repunit in the
# coalescent-simulated data
coal_Krs <- tibble(
  repunit = c("RU 1", "RU 2", "RU 3"),
  Kr = c(2, 3, 12)
)


# put those all together
Kr_tib <- bind_rows(alewife_Krs, coal_Krs)



# now, clean up the stuff into a single data frame
all_res <- bind_rows(list(`Leave-One-Out` = LOO, `Cross-Validation` = MCCV, Coalescent = COAL), .id = "sim") %>%
  mutate(repunit = recode(repunit, 
                          `N New England` = "NNE", 
                          `S New England` = "SNE", 
                          `Mid-Atlantic` = "MAT",
                          `Reporting Unit 1` = "RU 1",
                          `Reporting Unit 2` = "RU 2",
                          `Reporting Unit 3` = "RU 3"
                          )) %>%
  mutate(method = recode(method, MCMC = "PM"))  %>%
  left_join(Kr_tib) %>%
  select(sim, repunit, Kr, everything())





# now we just format that appropriately for the table
values <- all_res %>%
  select(-mean_prop_bias) %>%
  unite(col = "temp", MSE, mean_bias, sep = "#") %>%
  spread(key = method, value = temp) %>%
  separate(PB, into = c("pb_mse", "pb_mr"), sep = "#", convert = TRUE) %>%
  separate(PM, into = c("pm_mse", "pm_mr"), sep = "#", convert = TRUE) %>%
  mutate(pbpm_mse = pb_mse / pm_mse,
         pbpm_mr = pb_mr / pm_mr) %>%
  mutate(space = "  ") %>%
  select(sim, repunit, Kr, pm_mse, pb_mse, pbpm_mse, space, pm_mr, pb_mr, pbpm_mr)


# now, format those better
vchars <- values

cols <- c(4, 5, 6, 8, 9, 10)
ds <- c(4, 4, 2, 3, 3, 2)
vchars[cols] <- lapply(seq_along(cols), function(i) {
  d <- ds[i]  
  fmt = paste0("%.", d, "f")
  sprintf(vchars[[cols[i]]], fmt = fmt)
}) 

table_entries <- vchars %>%
  mutate(sim = factor(sim, levels = c("Coalescent", "Leave-One-Out", "Cross-Validation"))) %>%
  mutate(sim = fct_recode(sim, `MC Cross-Validation` = "Cross-Validation")) %>%
  arrange(sim, Kr) %>%
  mutate(space1 = "  ") %>%
  select(sim:Kr, space1, everything())
    

# now we can write that to a file and it can be pasted in as the 
# body of the table in the LaTeX document.
# A little more processing is then required to make some blank lines, etc.
# but the values are all there.
write.table(table_entries, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "  &  ", eol = "   \\\\\n")
  
  
