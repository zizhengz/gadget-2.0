factor_to_numeric = function(f) {
  stopifnot(is.factor(f))
  f = droplevels(f)
  lv = levels(f)
  is_all_numeric = all(grepl("^[-+]?[0-9]*\\.?[0-9]+$", lv))
  if (is_all_numeric) {
    as.numeric(as.character(f))
  } else {
    as.numeric(f)
  }
}
