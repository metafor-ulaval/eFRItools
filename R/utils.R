get_mode <- function(x) {
  unique_val <- unique(x)
  unique_val[which.max(tabulate(match(x, unique_val)))]
}

assign.common.name <- function(sp_abbrev){
  sp_abbrev  <- toupper(sp_abbrev)

  dict <- data.frame(SB = "black spruce",    LA = "eastern larch",       BW = "white birch",     BF = "balsam fir",
                     CE = "cedar",           SW = "white spruce",        PT = "trembling aspen", PJ = "jack pine",
                     PO = "poplar",          PB = "balsam poplar",       PR = "red pine",        PW = "white pine",
                     SX = "spruce",          MR = "red maple",           AB = "black ash",       BY = "yellow birch",
                     OR = 'red oak',         CW = 'eastern white cedar', MH = 'hard maple',      HE = 'eastern hemlock',
                     BD = 'basswood',        CB = 'black cherry',        BE = 'american beech',  AW = 'white ash',
                     PL = 'largetooth aspen',AG = 'red ash',             OW = 'white oak',       IW = 'ironwood',
                     OB = 'bur oak',         EW = 'white elm',           MS = 'silver maple',    PS = 'scots pine',
                     OH = 'other hardwoods', BG = 'grey birch',          AL = 'alder',           SR = 'red spruce',
                     BB = 'blue beech',      MT = 'mountain maple',      MB = 'black maple',     OC = 'other conifers',
                     SN = 'norway spruce',   PE = 'silver poplar',       HI = 'hickory',         AX = 'ash') |>
    tidyr::pivot_longer(everything(), names_to = "abb", values_to = "common")

  dict$common[match(sp_abbrev, dict$abb)]
}
