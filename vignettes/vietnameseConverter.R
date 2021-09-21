## ----include=FALSE------------------------------------------------------------
library(DT)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("vietnameseConverter")

## ----eval = TRUE--------------------------------------------------------------
library(vietnameseConverter)

## -----------------------------------------------------------------------------
string_garbled <- c("Qu¶ng TrÞ", "An §«n", "Thõa Thiªn HuÕ")
string_garbled

## -----------------------------------------------------------------------------
tmp <- decodeVN(string_garbled)
tmp

## -----------------------------------------------------------------------------
decodeVN(string_garbled, diacritics = FALSE)

## -----------------------------------------------------------------------------
string_garbled_vps <- c("Quäng TrÎ",  "An ñôn", "ThØa Thiên Hu‰")
decodeVN(string_garbled_vps, from = "VPS")

## -----------------------------------------------------------------------------
string_tcvn_to_vps <- decodeVN(x = string_garbled, from = "TCVN3", to = "VPS")
string_tcvn_to_vps

## -----------------------------------------------------------------------------
decodeVN(string_tcvn_to_vps, from = "VPS")

## -----------------------------------------------------------------------------
data(vn_samples)

## -----------------------------------------------------------------------------
head(vn_samples$Unicode)

## -----------------------------------------------------------------------------
head(vn_samples$Unicode$Province_city)

## ----eval = FALSE-------------------------------------------------------------
#  View(vn_samples$Unicode)

## -----------------------------------------------------------------------------
DT::datatable(vn_samples$Unicode)

## ----eval=FALSE, include=FALSE------------------------------------------------
#  DT::datatable(vn_samples$TCVN3)

## -----------------------------------------------------------------------------
DT::datatable(vn_samples$TCVN3)

## -----------------------------------------------------------------------------
# take data frames out of list for easier readability
df_unicode <- vn_samples$Unicode
df_tcvn3 <- vn_samples$TCVN3

# conversion from TCVN3 to Unicode (default)
df_tcvn3_converted <- decodeVN(df_tcvn3)

# print output
DT::datatable(df_tcvn3_converted)

## -----------------------------------------------------------------------------
all.equal(df_unicode, df_tcvn3_converted)

## -----------------------------------------------------------------------------
df_tcvn3_converted2 <- decodeVN(df_tcvn3, diacritics = FALSE)
DT::datatable(df_tcvn3_converted2)

## -----------------------------------------------------------------------------
df_viscii <- vn_samples$VISCII
DT::datatable(decodeVN(df_viscii, from = "VISCII"))

## -----------------------------------------------------------------------------
DT::datatable(decodeVN(df_viscii, from = "VISCII", diacritics = FALSE))

## ----eval = FALSE-------------------------------------------------------------
#  library(sf)
#  sf_object <- st_read(...)                      # load sf data set
#  sf_object_geometry <- st_geometry(sf_object)   # temporarily store geometry column
#  sf_drop_geometry(sf_object)                    # remove geometry column
#  sf_object_decoded <- decodeVN(sf_object)       # run decodeVN
#  st_set_geometry(sf_object_decoded, sf_object_geometry)  # assign geometry column to decoded data frame (to make it spatial again)
#  sf_object_decoded                                       # the sf object with decoded character columns

