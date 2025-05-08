trimester <- function(x,
                      with_year = FALSE,
                      type = "trimester",
                      academic_start = 1)
{
  if (length(academic_start) > 1) {
    stop("`academic_start` must be a singleton", call. = FALSE)
  }
  fs <- (academic_start - 1) %% 12
  shifted <- seq(fs, 11 + fs) %% 12 + 1
  m <- month(x)
  trimesters <- rep(1:3, each = 4)
  s <- match(m, shifted)
  q <- trimesters[s]
  if (is.logical(type)) {
    type <- if (type)
      "year.trimester"
    else "trimester"
  }
  if (with_year == TRUE) {
    type <- "year.trimester"
  }
  switch(
    type,
    trimester = q,
    `year_start/end` = ,
    year.trimester = {
      nxt_year_months <- if (fs != 0)
        (fs + 1):12
      y = year(x) + (m %in% nxt_year_months)
      out = y + (q / 10)
      if (type == "year_start/end") {
        out = sprintf("%d/%d T%d", y - 1, y %% 100, q)
      }
      out
    },
    date_first = ,
    date_last = {
      starting_months <- shifted[seq(1, length(shifted), 4)]
      final_years <- year(x) - (starting_months[q] > m)
      trimester_starting_dates <- make_date(year = final_years,
                                          month = starting_months[q],
                                          day = 1L)
      if (type == "date_first") {
        trimester_starting_dates
      } else if (type == "date_last") {
        lubridate::add_with_rollback(trimester_starting_dates, months(4)) -
          days(1)
      }
    },
    stop("Unsuported type ", type)
  )
}
