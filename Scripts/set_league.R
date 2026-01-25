set_league <- function(name) {
  
  stopifnot(name %in% names(league_info))
  
  cfg <- league_info[[name]]
  
  # Reset category globals
  cats <<- hit.cats <<- pitch.cats <<-
    hitlist.count <<- pitlist.count <<- NULL
  
  if (!is.null(cfg$cats)) {
    cats           <<- cfg$cats$all
    hit.cats      <<- cfg$cats$hit
    pitch.cats    <<- cfg$cats$pitch
    hitlist.count <<- cfg$cats$hit_ct
    pitlist.count <<- cfg$cats$pit_ct
  }
  
  if (!is.null(cfg$leagues)) {
    leagues <<- cfg$leagues
  } else {
    leagues <<- NULL
  }
  
  invisible(cfg)
}
