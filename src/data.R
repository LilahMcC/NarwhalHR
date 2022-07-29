

find_depth <- function(narwhal_id) {
  dir("data/raw_data/biologging", 
      pattern = sprintf("Depth_%s.txt", narwhal_id),
      full.names = TRUE,
      ignore.case = TRUE)
}

find_accx <- function(narwhal_id) {
  dir("data/raw_data/biologging", 
      pattern = sprintf("HDI[0-9]_%s.txt", narwhal_id),
      full.names = TRUE,
      ignore.case = TRUE)
}

find_accy <- function(narwhal_id) {
  dir("data/raw_data/biologging", 
      pattern = sprintf("HDJ[0-9]_%s.txt", narwhal_id),
      full.names = TRUE,
      ignore.case = TRUE)
}

find_accz <- function(narwhal_id) {
  dir("data/raw_data/biologging", 
      pattern = sprintf("HDK[0-9]_%s.txt", narwhal_id),
      full.names = TRUE,
      ignore.case = TRUE)
}


find_motionless <- function(narwhal_id) {
  dir("data/raw_data/motionless", 
      pattern = sprintf("%s_motionless.tsv", narwhal_id),
      full.names = TRUE,
      ignore.case = TRUE)
}

find_10hz <- function(narwhal_id) {
  dir("data/derived_data/biologging/", 
      pattern = sprintf("%s_10hz.rds", narwhal_id),
      full.names = TRUE, 
      ignore.case = TRUE)
}