# Build a blank loader table template from a Google Sheet column

# Args:
#   sheet_url  : Full Google Sheet URL
#   sheet      : Tab name
#   range      : Column range to read (default "C1:C")
#   source_df  : Data frame to size rows from (optional)
#   n_rows     : Explicit row count if no source_df (optional)
#   drop_first : Drop first cell (often a title/header). Default TRUE
#   clean      : Deduplicate + drop blanks + trim. Default TRUE
#
# Returns:
#   list(fields = <character>, template = <tibble>)

build_loader_template <- function(
    sheet_url,
    sheet,
    range      = "C1:C",
    source_df  = NULL,
    n_rows     = NULL,
    drop_first = TRUE,
    clean      = TRUE
) {
  # read the column (no headers)
  vars <- googlesheets4::read_sheet(
    sheet_url,
    sheet     = sheet,
    range     = range,
    col_names = FALSE
  )
 
  if (ncol(vars) == 0 || nrow(vars) == 0) {
    stop("Selected range has no data. Check `sheet`/`range`.")
  }
 
  fields <- vars[[1]]
  
  # drop the first cell if it’s a title/header
  if (isTRUE(drop_first) && length(fields) > 0) {
    fields <- fields[-1]
  }
  
  # clean: remove duplicates, remove blanks, remove extra whitespace
  if (isTRUE(clean)) {
    fields <- unique(fields)
    fields <- fields[!is.na(fields)]
    fields <- if (requireNamespace("stringr", quietly = TRUE)) {
      stringr::str_squish(fields)
    } else {
      trimws(gsub("\\s+", " ", fields))
    }
    fields <- fields[fields != ""]
  }
  
  if (length(fields) == 0) {
    stop("No field names found after cleaning. Check the sheet content.")
  }
  
  # decide row count
  if (!is.null(source_df)) {
    n <- nrow(source_df)
  } else if (!is.null(n_rows)) {
    n <- as.integer(n_rows)
    if (is.na(n) || n < 0) stop("`n_rows` must be a non-negative integer.")
  } else {
    n <- 0L
  }
  
  # build the template (all character NA by default)
  template <- tibble::as_tibble(
    stats::setNames(
      purrr::map(fields, ~ rep(NA_character_, n)),
      fields
    )
  )
  list(fields = fields, template = template)
}