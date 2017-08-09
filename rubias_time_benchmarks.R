
# This is an R script showing the steps taken to do some speed/timing benchmarks
# on `rubias` using a few different data sets chosen from across the spectrum of data sets
# that people might throw at `rubias`.  In each case we will create examine the time it
# takes to infer a mixture and do some LOO simulations.

# I was going to do this in a Notebook, but I am capturing the timings from the
# messages that infer_mixture() spits out and those don't get captured correctly
# in a notebook.

# The data sets that we will use are:

# 1. alewife microsatellites (lots of individuals, small number of microsatellites)
# 2. CA chinook SNPs (quite a few individuals, 100 SNPs)
# 3. Western Alaska chinook (>10K SNPs, small number of individuals)
# 4. DFO GBS SNP data (very many individuals with 300-400 SNPs)

# Most of these data sets are really reference samples.  Rather than try to find appropriate 
# mixture samples, we are going to simply sample individuals with replacement so that 
# we can simulate three mixtures of size 250 from each.  Obviously accuracy results obtained
# this way will be incorrect (because there is no leave-one-out happening...), but the 
# timing results will be fine.


library(tidyverse)
library(stringr)
library(rubias)


#### Helpful Functions ####
# For this we will make a simple function that does this random sampling from the reference data set
# and creates the three mixture data sets of size 250 individuals.

#' make a mixture from a reference.  This is not how things should
#' be done to assess accuracy, but it OK for timing
#' @param ref the reference data set to resample from
#' @param mixes number of mixtures
#' @param n size of each mixture
incorrectly_resampled_mixtures <- function(ref, mixes = 3, n = 250) {
  ret <- ref %>% 
    sample_n(size = mixes * n, replace = TRUE) %>%
    mutate(sample_type = "mixture",
           collection = rep(paste("mix", 1:mixes, sep = "-"), each = n),
           repunit = NA_character_,
           indiv = make.unique(paste("mix-", indiv)))
} 



# Now, we can also make a function that will run infer_mixture and  slurp
# the times out
#' @param ref reference
#' @param mix mixture
#' @param gsc gen_start_col
infer_mixture_and_grab_timings <- function(ref, mix, gsc) {
  messages <- capture.output(
    results <- infer_mixture(reference = ref, mixture = mix, gen_start_col = gsc),
    type = "message"
  )
  
  # then process the messages into a tidy format and return it
  timings <- enframe(messages) %>% 
    separate(value, into = c("stage", "time"), sep = "time:") %>% 
    mutate(seconds = parse_number(time)) %>%
    mutate(phase = paste("mix-", cumsum(is.na(seconds)), sep = ""),
           phase = ifelse(phase == "mix-0", "prep", phase)) %>%
    filter(!is.na(seconds)) %>%
    mutate(stage = str_trim(stage, side = "both")) %>%
    select(phase, stage, seconds)
  
  list(timings = timings, results = results)
}


# for example, to get the timings for alewife we would do:
set.seed(5)
ale_mix <- incorrectly_resampled_mixtures(alewife)

ale_timings <- infer_mixture_and_grab_timings(ref = alewife, mix = ale_mix, gsc = 17)

# that output is tidy so it will be easy to summarise



#### Compiling the Data sets ####

# We will compile all these into a named list.  We can start with alewife
set.seed(100)
datasets <- list()
datasets$alewife <- list(ref = alewife, mix = incorrectly_resampled_mixtures(alewife), gsc = 17,
                         ncoll = length(unique(alewife$collection)), nref = length(unique(alewife$indiv)),
                         nloci = (ncol(alewife) - 17 + 1)/2)

# then do the same for CA chinook
datasets$cal_chinook <- list(ref = chinook, mix = incorrectly_resampled_mixtures(chinook), gsc = 5,
                             ncoll = length(unique(chinook$collection)), nref = length(unique(chinook$indiv)),
                             nloci = (ncol(chinook) - 5 + 1)/2)



# for the W AK chinook, I have made a parseable two column file of 
# it in this repo.
wak <- read_tsv("data/wak2col.txt.gz", na = "00")

wak_ready <- wak %>%
  rename(indiv = id) %>%
  mutate(collection = str_replace_all(indiv, "[0-9_]*", ""),
         repunit = collection,
         sample_type = "reference") %>%
  select(sample_type, repunit, collection, indiv, everything())

datasets$ak_chinook <- list(ref = wak_ready, mix = incorrectly_resampled_mixtures(wak_ready), gsc = 5,
                            ncoll = length(unique(wak_ready$collection)), nref = length(unique(wak_ready$indiv)),
                            nloci = (ncol(wak_ready) - 5 + 1)/2)


# and finally we can do it for the DFO data 
if (file.exists("data/bco_baseline.rds")) {
  dfo <- read_rds(path = "data/bco_baseline.rds") %>%
    mutate(collection = as.character(collection),
           repunit = collection)
  
  datasets$can_coho <- list(ref = dfo, mix = incorrectly_resampled_mixtures(dfo), gsc = 5,
                            ncoll = length(unique(dfo$collection)), nref = length(unique(dfo$indiv)),
                            nloci = (ncol(dfo) - 5 + 1)/2)
}




#### then run the timings on all that ####
full_results <- lapply(datasets, function(x) infer_mixture_and_grab_timings(ref = x$ref, mix = x$mix, gsc = x$gsc))


# tidy up the results 
tidy_timings <- lapply(names(full_results), function(n) {
  full_results[[n]]$timings %>%
    mutate(ncoll = datasets[[n]]$ncoll,
           nref = datasets[[n]]$nref,
           nloci = datasets[[n]]$nloci,
           data = n)
}) %>%
  bind_rows()


#### now make a table of it ####
tab_tibble <- tidy_timings %>%
  mutate(what = ifelse(phase == "prep", "Data Prep (secs)", "Inference (secs)")) %>%
  group_by(data, nref, ncoll, nloci, what) %>% 
  summarise(seconds = sprintf("%.1f", sum(seconds))) %>%
  spread(key = what, value = seconds) %>%
  arrange(`Data Prep (secs)`)

# now, I am going to print that to a latex table that I can insert into the paper
write.table(tab_tibble, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "  &  ", eol = "  \\\\\n")
