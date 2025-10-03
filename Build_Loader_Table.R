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
}