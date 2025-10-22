get_mode <- function(x) {

  unique_val <- unique(x)
  unique_val[which.max(tabulate(match(x, unique_val)))]

}



# UTILE ????????????????
assign_common_name <- function(sp_abbrev){

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
    tidyr::pivot_longer(dplyr::everything(), names_to = "abb", values_to = "common")

  dict$common[match(sp_abbrev, dict$abb)]

}
# UTILE ????????????????

# UTILE ????????????????
assign_type <- function(sp_common){

  sp_common  <- tolower(sp_common)
  ifelse(stringr::str_detect(sp_common, pattern = "pine|spruce|fir|cedar|larch|conifers|hemlock"), "Coniferous", "Deciduous")

}
# UTILE ????????????????

sum_cover <- function(x){

  list(x |>
         dplyr::group_by(value) |>
         dplyr::summarise(class_area = sum(coverage_area)))

}

imputation <- function(reference_polygons,
                       var,
                       nn){

  # Extract values of the variable for the reference data
  data <- reference_polygons[[var]]

  # The apply function allows, for each row of the nn matrix which is a target polygon, to extract the observations (values in nn matrix are index) of the reference polygons
  if (is.numeric(data))
    # For numeric values, the mean of the k observations from reference data is used to impute value into the target polygon
    inputation_result <- apply(nn, MARGIN = 1, FUN = function(x){ mean(data[x]) })
  else if (is.factor(data) || is.character(data))
    # For categorical values, the mode of the k observations from reference data is used to impute value into the target polygon
    inputation_result <- apply(nn, MARGIN = 1, FUN = function(x){ get.mode(data[x]) })
  else
    stop(paste("Non supported column type", var))

  return(inputation_result)
}
