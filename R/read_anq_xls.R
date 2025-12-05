read_file_anq <- function(
  path_file,
  datasets = c("MB", "MP", "PH", "PB", "FM"),
  marker = "Ab hier Eingabe",
  preview_rows = 30
) {
  # Identify all sheets in the Excel file and classify them by dataset type.
  # The classification is based on pattern matching within sheet names.

  sheets <- data.frame(
    name = readxl::excel_sheets(path_file),
    position = seq_along(readxl::excel_sheets(path_file))
  ) |>
    dplyr::mutate(
      dataset = dplyr::case_when(
        stringr::str_detect(
          name,
          stringr::regex("MB", ignore_case = TRUE)
        ) ~ "MB",
        stringr::str_detect(
          name,
          stringr::regex("MP", ignore_case = TRUE)
        ) ~ "MP",
        stringr::str_detect(
          name,
          stringr::regex("PH", ignore_case = TRUE)
        ) ~ "PH",
        stringr::str_detect(
          name,
          stringr::regex("PB", ignore_case = TRUE)
        ) ~ "PB",
        stringr::str_detect(
          name,
          stringr::regex("FM", ignore_case = TRUE)
        ) ~ "FM",
        TRUE ~ NA_character_
      )
    )

  # Define a helper to read one dataset (e.g., "MB", "MP", etc.) from the file.
  # It locates the correct sheet, determines where the data starts, and reads it.

  read_dataset <- function(dataset_code) {
    # Find the sheet position associated with this dataset
    sheet_pos <- sheets |>
      dplyr::filter(dataset == dataset_code) |>
      dplyr::pull(position)

    # Skip if the dataset is not found in the file
    if (length(sheet_pos) == 0) {
      return(NULL)
    }
    sheet_pos <- sheet_pos[1]

    # Load a small preview of the first column to locate the marker text
    preview <- readxl::read_xlsx(
      path = path_file,
      sheet = sheet_pos,
      range = sprintf("A1:A%d", preview_rows),
      col_names = FALSE
    )

    # Identify the row containing the marker (case-insensitive)

    marker_row <- which(stringr::str_detect(
      preview[[1]],
      stringr::fixed(marker, ignore_case = TRUE)
    ))

    # Skip all rows up to and including the marker row
    skip_rows <- if (length(marker_row) > 0) {
      marker_row[1]
    } else {
      0
    }

    # Read the full sheet, starting one row below the marker if found
    readxl::read_xlsx(
      path = path_file,
      sheet = sheet_pos,
      skip = skip_rows,
      col_types = "text" # Read all columns as text to avoid type issues
    )
  }

  # Read all requested datasets from the file and return them as a named list
  purrr::set_names(datasets) |>
    purrr::map(read_dataset)
}
