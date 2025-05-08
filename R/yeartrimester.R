#' Represent year-trimester
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Create or coerce using `yeartrimester()`.
#'
#' @inheritSection yearmonth Display
#'
#' @inheritParams yearmonth
#'
#' @return year-trimester (`yeartrimester`) objects.
#'
#' @seealso [scale_x_yeartrimester] and others for ggplot2 scales
#' @family index functions
#' @rdname year-trimester
#' @export
#' @examples
#' # coerce POSIXct/Dates to yeartrimester
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 trimester")
#' yeartrimester(x)
#' yeartrimester(x, academic_start = 6)
#'
#' # parse characters
#' yeartrimester(c("2018 T1", "2018 tri1", "2018 trimester 1"))
#'
#' # seq() and arithmetic
#' tri <- yeartrimester("2017 Q1")
#' seq(tri, length.out = 10, by = 1) # by 1 trimester
#' tri + 0:9
#'
#' # display formats
#' format(tri, format = "%y tri%q")
yeartrimester <- function(x, academic_start = 1) {
  UseMethod("yeartrimester")
}

#' @rdname year-trimester
#' @param year,trimester A vector of numerics give years and Trimesters.
#' @export
#' @examples
#'
#' make_yeartrimester(year = 2021, trimester = 2:3)
#' make_yeartrimester(year = 2020:2021, trimester = 2:3)
make_yeartrimester <- function(year = 1970L, trimester = 1L, academic_start = 1) {
  lst <- vec_recycle_common(year = year, trimester = trimester)
  if (any(lst$trimester > 3 | lst$trimester < 1)) {
    abort("Trimesters can't be less than 1 or greater than 3.")
  }
  yeartrimester(3 * (lst$year - 1970) + lst$trimester - 1, academic_start)
}

#' @export
yeartrimester.default <- function(x, academic_start = 1) {
  dont_know(x, "yeartrimester")
}

#' @export
yeartrimester.NULL <- function(x, academic_start = 1) {
  new_yeartrimester(double(), academic_start = 1)
}

#' @export
yeartrimester.logical <- function(x, ...) {
  if (is.logical(x) && all(is.na(x))) {
    new_yeartrimester(0) + NA_real_
  } else {
    dont_know(x, "yeartrimester")
  }
}

#' @export
yeartrimester.POSIXct <- function(x, academic_start = 1) {
  yr <- year(x)
  mth <- academic_start + (month(x) - academic_start) %/% 4 * 4
  mth0 <- mth == 0
  mth1 <- mth == -1
  mth[mth0] <- 12
  mth[mth1] <- 11
  lgl <- mth0 | mth1
  vec_slice(yr, lgl) <- vec_slice(yr, lgl) - 1
  new_yeartrimester(make_date(yr, mth), academic_start)
}

#' @export
yeartrimester.POSIXlt <- yeartrimester.POSIXct

#' @export
yeartrimester.Date <- yeartrimester.POSIXct

#' @export
yeartrimester.character <- function(x, academic_start = 1) {
  # exact matching with q, tri, or trimester
  key_words <- regmatches(x, gregexpr("[[:alpha:]]+", x))
  if (all(grepl("^(t|tri|trimester)$", key_words, ignore.case = TRUE))) {
    yr_tri <- regmatches(x, gregexpr("[[:digit:]]+", x))
    digits_lgl <- map_lgl(yr_tri, function(.x) !has_length(.x, 2))
    digits_len <- map_int(yr_tri, function(.x) sum(nchar(.x)))
    if (any(digits_lgl) || any(digits_len != 5)) {
      abort("Character strings are not in a standard unambiguous format.")
    }
    yr_lgl <- map(yr_tri, function(.x) grepl("[[:digit:]]{4}", .x))
    yr <- as.integer(map2_chr(yr_tri, yr_lgl, function(.x, .y) .x[.y]))
    tri <- as.integer(map2_chr(yr_tri, yr_lgl, function(.x, .y) .x[!.y]))
    if (any(tri > 3 | tri < 1)) {
      abort("Trimesters can't be less than 1 or greater than 3.")
    }
    yeartrimester(3 * (yr - 1970) + tri - 1, academic_start)
  } else {
    assertDate(x)
    yeartrimester(anydate(x), academic_start)
  }
}

#' @export
yeartrimester.yearweek <- yeartrimester.POSIXct

#' @export
yeartrimester.yearmonth <- yeartrimester.POSIXct

#' @export
yeartrimester.yeartrimester <- function(x, academic_start = attr(x, "academic_start")) {
  fs <- academic_start(x)
  mth <- academic_start - fs
  new_yeartrimester(
    new_date(x) + period(year = -(fs == 1) + (academic_start == 1), month = mth),
    academic_start)
}

#' @export
yeartrimester.numeric <- function(x, academic_start = 1) {
  date0 <- make_date(1969 + as.integer(academic_start == 1), academic_start)
  new_yeartrimester(date0 + period(month = x * 4), academic_start)
}

#' @export
yeartrimester.yeartri <- function(x, academic_start = 1) {
  year <- trunc(x)
  last_month <- trunc((x %% 1) * 3 + 1) * 4
  first_month <- last_month - 2
  result <- make_date(year, first_month, 1)
  new_yeartrimester(result, academic_start)
}

new_yeartrimester <- function(x = double(), academic_start = 1) {
  if (!has_length(academic_start, 1)) {
    abort("`academic_start` must be of length 1.")
  }
  if (academic_start < 1 || academic_start > 12) {
    abort("`academic_start` only accepts a value between 1 and 12.")
  }
  new_vctr(x, academic_start = academic_start, class = "yeartrimester")
}

academic_start <- function(x) {
  attr(x, "academic_start") %||% 1
}

#' @rdname year-trimester
#' @export
is_yeartrimester <- function(x) {
  inherits(x, "yeartrimester")
}

#' @export
is.numeric.yeartrimester <- function(x) {
  FALSE
}

#' @export
tz.yeartrimester <- function(x) {
  "UTC"
}

# diff.yeartrimester <- function(x, lag = 1, differences = 1, ...) {
#   out <- diff((year(x) - 1970) * 4 + trimester(x),
#     lag = lag, differences = differences
#   )
#   structure(out, class = "difftime", units = "Trimesters")
# }

#' @rdname tsibble-vctrs
#' @method vec_cast yeartrimester
#' @export
vec_cast.yeartrimester <- function(x, to, ...) {
  UseMethod("vec_cast.yeartrimester")
}

#' @export
vec_cast.Date.yeartrimester <- function(x, to, ...) {
  new_date(x)
}

#' @export
vec_cast.POSIXct.yeartrimester <- function(x, to, ...) {
  as.POSIXct(new_date(x), ...)
}

#' @export
vec_cast.double.yeartrimester <- function(x, to, ...) {
  base <- yeartrimester(0, academic_start(x))
  4 * (year(x) - year(base)) + trimester(x) - trimester(base)
}

#' @export
vec_cast.POSIXlt.yeartrimester <- function(x, to, ...) {
  as.POSIXlt(new_date(x), ...)
}

#' @export
vec_cast.yeartrimester.yeartrimester <- function(x, to, ...) {
  yeartrimester(x, academic_start(to))
}

#' @export
vec_cast.character.yeartrimester <- function(x, to, ...) {
  format(x)
}

#' @rdname tsibble-vctrs
#' @method vec_ptype2 yeartrimester
#' @export
vec_ptype2.yeartrimester <- function(x, y, ...) {
  UseMethod("vec_ptype2.yeartrimester", y)
}

#' @export
vec_ptype2.yeartrimester.POSIXct <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.POSIXct.yeartrimester <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.yeartrimester.Date <- function(x, y, ...) {
  new_date()
}

#' @export
vec_ptype2.yeartrimester.yeartrimester <- function(x, y, ...) {
  if (academic_start(x) != academic_start(y)) {
    abort("Can't combine <yeartrimester> with different `academic_start`.")
  }
  new_yeartrimester(academic_start = academic_start(x))
}

#' @export
vec_ptype2.Date.yeartrimester <- function(x, y, ...) {
  new_date()
}

#' @rdname tsibble-vctrs
#' @method vec_arith yeartrimester
#' @export
vec_arith.yeartrimester <- function(op, x, y, ...) {
  UseMethod("vec_arith.yeartrimester", y)
}

#' @method vec_arith.yeartrimester default
#' @export
vec_arith.yeartrimester.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.yeartrimester numeric
#' @export
vec_arith.yeartrimester.numeric <- function(op, x, y, ...) {
  if (op == "+") {
    new_yeartrimester(as_date(x) + period(months = y * 4), academic_start(x))
  } else if (op == "-") {
    new_yeartrimester(as_date(x) - period(months = y * 4), academic_start(x))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yeartrimester yeartrimester
#' @export
vec_arith.yeartrimester.yeartrimester <- function(op, x, y, ...) {
  if (op == "-") {
    as.double(x) - as.double(y)
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.numeric yeartrimester
#' @export
vec_arith.numeric.yeartrimester <- function(op, x, y, ...) {
  if (op == "+") {
    yeartrimester(period(months = x * 4) + as_date(y), academic_start(y))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yeartrimester MISSING
#' @export
vec_arith.yeartrimester.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = x,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @export
format.yeartrimester <- function(x, format = "%Y T%q", ...) {
  fs <- academic_start(x)
  yrtri <- trimester(x, type = "year.trimester", academic_start = fs)
  yr <- trunc(yrtri)
  tri <- round(yrtri %% 1 * 10)
  tri_sub <- map_chr(formatC(tri), function(z) gsub("%q", z, x = format))
  tri_sub[is.na(tri_sub)] <- "-" # NA formats cause errors
  format.Date(make_date(yr, tri * 4 - 2), format = tri_sub)
}

#' @rdname tsibble-vctrs
#' @export
obj_print_data.yeartrimester <- function(x, ...) {
  if (length(x) == 0) return()
  print(format(x))
}

#' @export
obj_print_footer.yeartrimester <- function(x, ...) {
  cat_line("# Year starts on: ", fmt_month(academic_start(x)))
}

fmt_month <- function(x) {
  month.name[x]
}

#' @export
vec_ptype_abbr.yeartrimester <- function(x, ...) {
  "tri"
}

#' @export
seq.yeartrimester <- function(from, to, by, length.out = NULL, along.with = NULL,
                            ...) {
  fs <- academic_start(from)
  from <- vec_cast(from, new_date())
  if (!is_missing(to)) {
    to <- vec_cast(to, new_date())
  }
  if (is_missing(by)) {
    new_yeartrimester(seq_date(
      from = from, to = to, length.out = length.out,
      along.with = along.with, ...
    ), fs)
  } else {
    bad_by(by)
    by_tri <- paste(by, "trimester")
    new_yeartrimester(seq_date(
      from = from, to = to, by = by_tri, length.out = length.out,
      along.with = along.with, ...
    ), fs)
  }
}

#' @export
seq.ordered <- function(from, to, by, ...) {
  bad_by(by)
  lvls <- levels(from)
  idx_from <- which(lvls %in% from)
  idx_to <- which(lvls %in% to)
  idx <- seq.int(idx_from, idx_to, by = by)
  ordered(lvls[idx], levels = lvls)
}

#' @rdname year-trimester
#' @export
#' @examples
#'
#' # `academic_year()` helps to extract academic year
#' y <- yeartrimester(as.Date("2020-06-01"), academic_start = 6)
#' academic_year(y)
#' lubridate::year(y) # calendar years
academic_year <- function(x) {
  stopifnot(is_yeartrimester(x))
  trunc(trimester(x, TRUE, academic_start(x)))
}

#' @export
union.yeartrimester <- set_ops("yeartrimester", op = "union")

#' @export
intersect.yeartrimester <- set_ops("yeartrimester", op = "intersect")

#' @export
setdiff.yeartrimester <- set_ops("yeartrimester", op = "setdiff")

