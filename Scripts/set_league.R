set_league <- function(name) {
  cfg <- books[[name]]
  
  cats            <<- cfg$cats$all
  hit.cats       <<- cfg$cats$hit
  pitch.cats     <<- cfg$cats$pitch
  hitlist.count  <<- cfg$cats$hit_ct
  pitlist.count  <<- cfg$cats$pit_ct
  
  if (!is.null(cfg$leagues)) leagues <<- cfg$leagues
}
