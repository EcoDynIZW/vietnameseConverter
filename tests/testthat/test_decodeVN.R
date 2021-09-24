context("decodeVN")
library(vietnameseConverter)

data("vn_samples")



decode_example_tcvn3 <- decodeVN(x = vn_samples$TCVN3)


decode_example_vps <- decodeVN(x = vn_samples$VPS,
         from = "VPS")


decode_example_viscii <- decodeVN(x = vn_samples$VISCII,
         from = "VISCII")





# Test section


test_that("output matches Unicode input", {
  expect_true(all.equal(vn_samples$Unicode, decode_example_tcvn3))
  expect_true(all.equal(vn_samples$Unicode, decode_example_vps))
  expect_true(all.equal(vn_samples$Unicode, decode_example_viscii))
})

test_that("it works on vectors", {
  expect_true(all.equal(vn_samples$Unicode$Province_city, decodeVN(x = vn_samples$TCVN3$Province_city)))
  expect_true(all.equal(vn_samples$Unicode$Province_city, decodeVN(x = vn_samples$VPS$Province_city, from = "VPS")))
  expect_true(all.equal(vn_samples$Unicode$Province_city, decodeVN(x = vn_samples$VISCII$Province_city, from = "VISCII")))
})


test_that("NAs are handled correctly", {

  test <- vn_samples$TCVN3
  test$Province_city[1] <- NA

  expect_silent(test2 <- decodeVN(test))

  test_uni_na <- vn_samples$Unicode
  test_uni_na$Province_city[1] <- NA

  expect_true(all.equal(test_uni_na, test2))
})


test_that("back and forth conversion works", {
  tmp_tcvn <- decodeVN(x = vn_samples$Unicode, from = "Unicode", to = "TCVN3", diacritics = T)
  tmp_tcvn2unicode <- decodeVN(x = tmp_tcvn)
  expect_true(all.equal(tmp_tcvn2unicode, vn_samples$Unicode))

  tmp_viscii <- decodeVN(x = vn_samples$Unicode, from = "Unicode", to = "VISCII", diacritics = T)
  tmp_viscii2unicode <- decodeVN(x = tmp_viscii, from = "VISCII")
  expect_true(all.equal(tmp_viscii2unicode, vn_samples$Unicode))

  tmp_vps <- decodeVN(x = vn_samples$Unicode, from = "Unicode", to = "VPS", diacritics = T)
  tmp_vps2unicode <- decodeVN(x = tmp_vps, from = "VPS")
  expect_true(all.equal(tmp_vps2unicode, vn_samples$Unicode))
})


