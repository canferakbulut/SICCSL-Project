## packages
packages <- c("jsonlite", "LSAfun", "tidyr", "vader", "data.table", "rlist", "ngram", 
              "lme4", "stringr", "lubridate")
lapply(packages, require, character.only = TRUE)

## read csv
pat <- #<FILE_PATH>
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

## cleaning

#rm orphan comments
dt <- dt[!is.na(parent_index_w_na), ]
#rm deleted users
dt <- dt %>% filter(!grepl("\\[deleted\\]", dyad_pairs))

#rm links, new spaces, and text within square brackets
text_clean <- function(text){
  text <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", text)
  text <- gsub("\n+", " ", text)
  text <- gsub("&amp;#x200B;", "", text)
  text <- gsub("\\[[^][]*]", "", text)
  text <- gsub("^[[:space:]]*", "", text)
  return(text)
}

dt <- dt %>% rowwise() %>% mutate(text_clean = text_clean(text), parent_text_clean = text_clean(parent_text))

## add words 
nwords <- function(string, pseudo=T){
    ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

## get nwords and convert time
dt <- dt %>% rowwise() %>% mutate(text_nwords = nwords(text_clean), parent_nwords = nwords(parent_text_clean),
                                  text_date = as_datetime(timestamp), parent_text_date = as_datetime(as.numeric(parent_time)))
dt <- dt %>% mutate(difference_nwords = abs(parent_nwords - text_nwords))

## turn id 
count <- dt %>% group_by(dyad_pairs, conversation_id) %>% count() %>% as.data.frame()
dt <- dt[order(dt$text_date), ]

turns <- rep(NA, nrow(dt))
for(i in seq_len(nrow(count))){
  print(i)
  len <- count$n[i]
  dt.index <- which(dt$dyad_pairs == count$dyad_pairs[i] & dt$conversation_id == count$conversation_id[i])
  turn_number <- 1:len
  for(j in seq_along(turn_number)){
    turns[dt.index[j]] <- turn_number[j]
  }
}
dt <- dt %>% mutate(turn_number = turns)

## filter out adjacency pairs where either has nword < 5
dt <- dt %>% filter(!(text_nwords < 5 | parent_nwords < 5))

## filter out dyad conversations with length < 6
convo_remove <- count %>% filter(n <= 6) %>% select(dyad_pairs, conversation_id)
convo_index <- !(dt$dyad_pairs %in% convo_remove$dyad_pairs & dt$conversation_id %in% convo_remove$conversation_id)
dt <- dt[convo_index, ]

## remove duplicate & unneeded columns 
dt <- dt %>% select(-timestamp, -meta.gilded, -vectors)














