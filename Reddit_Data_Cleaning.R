## packages
init <- function(){
  packages <- c("jsonlite", "LSAfun", "tidyr", "vader", "data.table", "rlist", "ngram", 
              "lme4", "stringr", "lubridate")
  lapply(packages, require, character.only = TRUE)
}

## read csv
read_in <- function(file_path){
  df <- read.csv(file_path)
  dt <- data.table(df)
  return(dt)
}

## find parent location
parent_index_search <- function(parent_id, dt) {
  return(dt[parent_id, which = TRUE])
}

find_parent <- function(dt){
  print("finding parent ids!")
  setkey(dt, id)
  parent_index <- sapply(dt$reply_to, parent_index_search, dt = dt)
  print("finished finding parents!")
  return(parent_index)
}
  
## make dyad pair names

dyad_pair_name <- function(a1, a2){
  dyad <- a1 %>% c(a2) %>% sort() %>% paste(collapse = ":") %>% unname()
  return(dyad)
}

## add adjacency pairs & dyad pair names
add_parent_cols <- function(parent_index, dt) {
  print("adding parent columns!")
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
  parent_df <- as.data.frame(list.cbind(parent))
  print("finished adding parent columns!")
  return(cbind(dt, parent_df))
}


## cleaning

text_clean <- function(text){
  text <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", text)
  text <- gsub("\n+", " ", text)
  text <- gsub("&amp;#x200B;", "", text)
  text <- gsub("\\[[^][]*]", "", text)
  text <- gsub("^[[:space:]]*", "", text)
  return(text)
}

initial_clean <- function(parent_index, dt) {
  print("cleaning initial data!")
  #rm orphan comments
  dt <- dt[!is.na(parent_index), ]
  #rm deleted users
  dt <- dt %>% filter(!grepl("\\[deleted\\]", dyad_pairs))
  #rm links, new spaces, and text within square brackets
  dt <- dt %>% rowwise() %>% mutate(text_clean = text_clean(text), parent_text_clean = text_clean(parent_text))
  print("cleaned initial data!")
  return(dt)
}

## add words 
nwords <- function(string, pseudo=T){
    ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

## get nwords and convert time
add_nwords <- function(dt){
  print("adding word count variables!")
  dt <- dt %>% rowwise() %>% mutate(text_nwords = nwords(text_clean), parent_nwords = nwords(parent_text_clean))
  dt <- dt %>% mutate(difference_nwords = abs(parent_nwords - text_nwords))
  print("added word count variables!")
  return(dt)
}

convert_utc_to_datetime <- function(dt){
  print("converting utc to datetime!")
  dt <- dt %>% rowwise() %>% mutate(text_date = as_datetime(timestamp), parent_text_date = as_datetime(as.numeric(parent_time)))
  print("converted utc to datetime!")
  return(dt)
}


## turn id 
count_df <- function(dt){ 
  print("making count df!")
  count <- dt %>% group_by(dyad_pairs, conversation_id) %>% count() %>% as.data.frame()
  print("finished count df!")
  return(count)
  }

add_convo_turn_id <- function(count, dt){
  print("starting to make turn ids - this might take a while...")
  dt <- dt[order(dt$text_date), ]
  
  turns <- rep(NA, nrow(dt))
    for(i in seq_len(nrow(count))){
      if(i %% 5 == 0) {print(paste0("on the ", i, "th turn id!"))}
      len <- count$n[i]
      dt.index <- which(dt$dyad_pairs == count$dyad_pairs[i] & dt$conversation_id == count$conversation_id[i])
      turn_number <- 1:len
      turns[dt.index] <- turn_number
    }
  
    dt$turn_number <- turns
    print("finished turn ids!")
    return(dt)
}

last_clean <- function(count, dt) {
  print("starting last clean!")
  ## filter out adjacency pairs where either has nword < 5
  dt <- dt %>% filter(!(text_nwords < 5 | parent_nwords < 5))
  convo_remove <- count %>% filter(n <= 6) %>% select(dyad_pairs, conversation_id)
  convo_index <- !(dt$dyad_pairs %in% convo_remove$dyad_pairs & dt$conversation_id %in% convo_remove$conversation_id)
  ## filter out dyad conversations with length < 6
  dt <- dt[convo_index, ]
  ## remove duplicate & unneeded columns 
  dt <- dt %>% select(-timestamp, -meta.gilded, -vectors)
  print("finished last clean! enjoy!")
  return(dt)
}

main <- function(filepath){
  init()
  dt <- read_in(filepath)
  parent_index <- find_parent(dt)
  dt <- add_parent_cols(parent_index, dt)
  dt <- initial_clean(parent_index, dt)
  dt <- add_nwords(dt)
  dt <- convert_utc_to_datetime(dt)
  count <- count_df(dt)
  dt <- add_convo_turn_id(count, dt)
  dt <- last_clean(count, dt)
  return(dt)
}














