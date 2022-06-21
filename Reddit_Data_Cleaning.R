## packages
packages <- c("jsonlite", "LSAfun", "dplyr", "vader", "data.table", "rlist", "ngram", "lme4")
lapply(packages, require, character.only = TRUE)

## read csv
pat <- "small_reddit_convo.csv"
df <- read.csv(pat)
dt <- data.table(df)

## find parent location
setkey(dt, id)

parent_index_search <- function(parent_id, dt) {
  return(dt[parent_id, which = TRUE])
}

parent_index_w_na <- sapply(dt$reply_to, parent_index_search, dt = dt)

## make dyad pair names

dyad_pair_name <- function(a1, a2){
  dyad <- a1 %>% c(a2) %>% sort() %>% paste(collapse = ":") %>% unname()
  return(dyad)
}

## add adjacency pairs & dyad pair names
add_parent_cols <- function(parent_index, dt) {
  n <- nrow(dt)
  filler <- rep(NA, n)
  na.index <- !is.na(parent_index)
  parent <- list('dyad_pairs' = filler,
                 'parent_text' = filler,
                 'parent_time' = filler, 
                 'parent_score' = filler, 
                 'parent_name' = filler)
  
  for(i in seq_len(n)){
    if(na.index[i]){
      x <- parent_index[i]
      parent[['parent_text']][i] <- dt$text[x]
      parent[['parent_time']][i] <- dt$timestamp[x]
      parent[['parent_score']][i] <- dt$meta.score[x]
      parent[['parent_name']][i] <- dt$speaker[x]
      parent[['dyad_pairs']][i] <- dyad_pair_name(dt$speaker[i], dt$speaker[x])
      }
    }
  return(as.data.frame(list.cbind(parent)))
}

dt <- cbind(dt, add_parent_cols(parent_index_w_na, dt))

## rm orphan comments
dt <- dt[!is.na(parent_index_w_na), ]
dt <- dt %>% filter(!grepl("\\[deleted\\]", dyad_pairs))

