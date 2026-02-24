

league_info <- list(
  nfbc = list(
    leagues = c("OC","DC"),
    dc = "1PjKvrQ6l4ZegRx5hUhmjZD9eJkB3lK2Ar-ZkvSB5Dwk",
    oc = "1Mulvq-EdPubvXxhdQ_8eigYfZus74GiTeG3wIkxukQo",
    cats = list(
      all = c("r","hr","rbi","sb","avg","w","k","sv","era","whip"),
      hit = c("r","hr","rbi","sb","avg"),
      pitch = c("w","k","sv","era","whip"),
      hit_ct = c("r","hr","rbi","sb"),
      pit_ct = c("w","k","sv")
    )
  )
  , ottoneu = list(
      sheet = "Custom value inputs - FG",
      bat_split = 0.67, 
      
      leagues = list(
          FnL = list(
            teams = 12,
            sheet_link = "1lJAGdHExdjuUx1Fu2ZDXn2uyRmPmd11Hj4gN4rh41KU"
          )
        , `20SD` = list(
          teams = 20,
          sheet_link = "1Sp_XjplprhI-kyd5qwX3LRyLYCWOLr2Kv-1TRLuYDZo"
        )
      )
    )
  , hc = list(
        
      teams = 12
      , sheet_link = "1jy9PzHz_EuS4zjXkXdeCrpx-EaEyHbAaEgtqHfmItjo"
    , cats = list(
      all = c("r","hr","rbi","sb","avg","ops","qs","k","svhld","era","hip","bb9"),
      hit = c("r","hr","rbi","sb","avg","ops"),
      pitch = c("qs","k","svhld","era","hip","bb9"),
      hit_ct = c("r","hr","rbi","sb"),
      pit_ct = c("qs","k","svhld")
    )
  )
  ,   mendoza = list(
    sheet = "Shane Projections",
    
    # global defaults
    bat_split = 0.54,
    rp_nerf   = 0.65,
    
    leagues = list(
      main = list(
        teams = 30,
        sheet_link = "1GFZTsccSIEr0yqVdE6md8hrUxj9FpalBlePv0NkBuqE",
        
        # optional per-league overrides
        # bat_split = 0.53,
        # rp_nerf   = 0.60,
        
        fantrax_exclude = "*06als*",
        
        fantrax_overrides = c(
          "13770" = "*02mzf*",
          "17170" = "*031fj*",
          "26203" = "*0514i*",
          "17871" = "*03qpg*",
          "13346" = "*02n0v*"
        )
      )
    )
  )
)



