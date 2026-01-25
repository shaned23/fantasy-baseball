## -------------------------
## Mendoza Export
## -------------------------

export_mendoza <- function(
    bat_split = NULL,
    rp_nerf   = NULL
) {
  
  cfg <- league_info$mendoza
  
  purrr::walk(names(cfg$leagues), function(lg) {
    
    lg_cfg <- cfg$leagues[[lg]]
    
    ## Resolve parameters (export → league → global)
    bs <- if (!is.null(bat_split) && lg %in% names(bat_split)) {
      bat_split[[lg]]
    } else if (!is.null(lg_cfg$bat_split)) {
      lg_cfg$bat_split
    } else {
      cfg$bat_split
    }
    
    rp <- if (!is.null(rp_nerf) && lg %in% names(rp_nerf)) {
      rp_nerf[[lg]]
    } else if (!is.null(lg_cfg$rp_nerf)) {
      lg_cfg$rp_nerf
    } else {
      cfg$rp_nerf
    }
    
    ## Build values
    out <- mendoza.values(
      teams     = lg_cfg$teams,
      rp.nerf   = rp,
      bat.split = bs
    ) %>%
      select(fg.id, name, pos, value) %>%
      left_join(
        distinct(playerid.map, fg.id, fantrax.id) %>%
          filter(fantrax.id != ""),
        by = "fg.id"
      ) %>%
      rename(
        Name     = name,
        Position = pos,
        Dollars  = value
      ) %>%
      select(fantrax.id, Name, Position, Dollars, fg.id) %>%
      filter(
        fantrax.id != lg_cfg$fantrax_exclude | is.na(fantrax.id)
      ) %>%
      mutate(
        fantrax.id = case_when(
          fg.id %in% names(lg_cfg$fantrax_overrides)
          ~ unname(lg_cfg$fantrax_overrides[fg.id]),
          TRUE ~ fantrax.id
        )
      )
    
    ## Export
    googlesheets4::write_sheet(
      out,
      ss    = lg_cfg$sheet_link,
      sheet = cfg$sheet
    )
    
    message(
      sprintf(
        "Exported Mendoza [%s] | teams=%d | bat_split=%.2f | rp_nerf=%.2f",
        lg, lg_cfg$teams, bs, rp
      )
    )
  })
}

export_mendoza()
