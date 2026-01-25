export_ottoneu <- function(bat_split = NULL) {
  
  ott_cfg <- league_info$ottoneu
  
  purrr::walk(names(ott_cfg$leagues), function(lg) {
    
    lg_cfg <- ott_cfg$leagues[[lg]]
    
    # Resolve bat_split
    bs <- if (!is.null(bat_split) && lg %in% names(bat_split)) {
      bat_split[[lg]]
    } else if (!is.null(lg_cfg$bat_split)) {
      lg_cfg$bat_split
    } else {
      ott_cfg$bat_split
    }
    
    df <- ottoneu.values(
      teams     = lg_cfg$teams,
      bat.split = bs
    )
    
    out <- df %>%
      mutate(
        Prospect = +(stringr::str_sub(fg.id, 1, 2) == "sa" & value < 1)
      ) %>%
      select(
        name, pos, value, fg.id, ottoneu.id,
        Prospect, total.pts, `g/ip`, `ppg/ip`
      ) %>%
      rename(
        Name         = name,
        Position     = pos,
        Dollars      = value,
        PlayerID     = fg.id,
        `ottoneu ID` = ottoneu.id
      )
    
    googlesheets4::write_sheet(
      out,
      ss    = lg_cfg$sheet_link,
      sheet = ott_cfg$sheet
    )
    
    message(
      sprintf("Exported %s | teams=%d | bat_split=%.2f",
              lg, lg_cfg$teams, bs)
    )
  })
}

# Default = 0.67
export_ottoneu()

# export_ottoneu(
#   bat_split = c(
#     FnL  = 0.62,
#     `20SD` = 0.70
#   )
# )

