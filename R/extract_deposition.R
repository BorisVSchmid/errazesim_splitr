#' Read HYSPLIT Binary Deposition Output via con2asc Conversion
#'
#' This function converts a HYSPLIT binary concentration/deposition output file
#' (like 'cdump' or 'output.bin') to ASCII using the external 'con2asc.exe'
#' utility and then reads the resulting ASCII file into a tidy data frame.
#'
#' Note: This function requires a working HYSPLIT installation and access to
#' the 'con2asc.exe' executable. The conversion step can be slow for large files.
#'
#' @param con2asc_path Character string. The full path to the 'con2asc.exe'
#'   executable.
#' @param input_bin_path Character string. The full path to the input HYSPLIT
#'   binary concentration/deposition file (e.g., "path/to/output.bin").
#' @param output_ascii_path Character string. The desired full path for the
#'   output ASCII file (e.g., "path/to/output.txt"). It's recommended to match
#'   the name con2asc will create (e.g., use "path/to/output.bin.txt" if
#'   input is "path/to/output.bin").
#' @param overwrite Logical. If TRUE, forces the recreation of the ASCII file
#'   by running con2asc even if the output file already exists. Defaults to FALSE.
#'
#' @return A data frame (tibble) with columns: YEAR, MO, DA, HR, LAT, LON,
#'   and Deposition (numeric). Rows with non-positive or NA deposition values
#'   are filtered out.
#'
#' @importFrom utils read.table
#' @importFrom dplyr mutate filter rename select
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage (default - don't overwrite):
#' deposition_df <- extract_deposition(
#'   con2asc_path = "C:/hysplit/exec/con2asc.exe",
#'   input_bin_path = "C:/hysplit_runs/run1/output.bin",
#'   output_ascii_path = "C:/hysplit_runs/run1/output.bin.txt"
#' )
#'
#' # Example usage (force overwrite):
#' deposition_df_overwrite <- extract_deposition(
#'   con2asc_path = "C:/hysplit/exec/con2asc.exe",
#'   input_bin_path = "C:/hysplit_runs/run1/output.bin",
#'   output_ascii_path = "C:/hysplit_runs/run1/output.bin.txt",
#'   overwrite = TRUE
#' )
#'
#' # Check the first few rows
#' head(deposition_df)
#'
#' # Load sf and ggplot2 for plotting
#' library(sf)
#' library(ggplot2)
#'
#' deposition_sf <- sf::st_as_sf(deposition_df,
#'                              coords = c("LON", "LAT"),
#'                              crs = 4326)
#'
#' ggplot(deposition_sf) +
#'   geom_sf(aes(color = Deposition)) +
#'   scale_color_viridis_c() +
#'   theme_minimal()
#' }
extract_deposition <- function(con2asc_path,
                               input_bin_path,
                               output_ascii_path,
                               overwrite = FALSE) { # Added overwrite argument

  # --- Determine the path con2asc is likely to create ---
  # con2asc with -s usually creates {input_file_name}.txt
  # We use this to check existence and potentially read from it.
  expected_con2asc_output <- sub("\\.[^.]*$", ".bin.txt", input_bin_path, perl = TRUE) # Heuristic

  # --- Decide whether to run con2asc or read existing file ---
  run_conversion <- TRUE # Assume we need to run conversion by default

  # Check if the expected output file exists and overwrite is FALSE
  if (!overwrite && file.exists(expected_con2asc_output)) {
    warning("ASCII file '", expected_con2asc_output, "' already exists and overwrite=FALSE. Reading existing file.", call. = FALSE)
    output_ascii_path <- expected_con2asc_output # Ensure we read the correct file
    run_conversion <- FALSE # Don't run conversion
  } else if (!overwrite && file.exists(output_ascii_path)) {
    # Check the user-specified path if the expected one didn't exist
    warning("ASCII file '", output_ascii_path, "' already exists and overwrite=FALSE. Reading existing file.", call. = FALSE)
    run_conversion <- FALSE # Don't run conversion
  }

  # --- Run con2asc if needed ---
  if (run_conversion) {
    if (overwrite && file.exists(expected_con2asc_output)) {
        print(paste("Overwrite=TRUE. Removing existing file:", expected_con2asc_output))
        file.remove(expected_con2asc_output)
    } else if (overwrite && file.exists(output_ascii_path)) {
         print(paste("Overwrite=TRUE. Removing existing file:", output_ascii_path))
         file.remove(output_ascii_path)
    }

    # Check if con2asc executable exists
    if (!file.exists(con2asc_path)) {
      stop("con2asc executable not found at: ", con2asc_path, call. = FALSE)
    }

    # Check if input binary file exists
    if (!file.exists(input_bin_path)) {
      stop("Input binary file not found at: ", input_bin_path, call. = FALSE)
    }

    warning("Attempting conversion using con2asc. This might be slow for large files.", call. = FALSE)

    # Construct command: Use -s for a single output file.
    # Ensure paths are quoted for safety.
    command <- paste0('"', con2asc_path, '" -i"', input_bin_path, '" -s')


    # Run the command
    status <- system(command)

    # Basic check for command execution success
    if (status != 0) {
      stop("con2asc command failed with status code: ", status,
           ". Check HYSPLIT messages or run manually.", call. = FALSE)
    }

    # Add a small delay just in case the filesystem needs a moment
    Sys.sleep(0.5)

    # Check if the expected output file was created by con2asc -s
     if (!file.exists(expected_con2asc_output)) {
        # If the heuristic name didn't work, check the user-provided name
        if (!file.exists(output_ascii_path)){
           stop("con2asc command seemed to run (status 0), but the expected output file '",
             expected_con2asc_output, "' or '", output_ascii_path, "' was not found.", call. = FALSE)
         } else {
           # If the user-provided name exists, use that path for reading
           expected_con2asc_output <- output_ascii_path
         }
     }
     # Ensure output_ascii_path points to the file to be read
     output_ascii_path <- expected_con2asc_output

    print(paste("con2asc command executed successfully. Reading:", output_ascii_path))
  } # End of run_conversion block

  # --- Read the resulting ASCII table ---
  # Use tryCatch for better error handling during file reading
  deposition_data <- tryCatch({
    utils::read.table(output_ascii_path,
                      header = FALSE,
                      skip = 1, # Skip the header row generated by con2asc
                      # Inferring column names based on standard con2asc output with -s
                      # Example header might be: # YR MO DY HR LAT LON VAL1 VAL2
                      # Make sure these match your actual con2asc output
                      col.names = c("YEAR", "MO", "DA", "HR", "LAT", "LON", "ZeroLayer", "DepositionLayer","TopLayer"),
                      stringsAsFactors = FALSE, # Prevent factors
                      encoding = "UTF-8", # Specify encoding if needed
                      blank.lines.skip = TRUE, # Skip blank lines if any
                      comment.char = "") # Avoid treating potential '#' as comments
  }, error = function(e) {
    stop("Failed to read the ASCII file '", output_ascii_path, "'. Error: ", e$message, call. = FALSE)
  })

  # --- Clean the data ---
  # Ensure dplyr functions are used if dplyr is intended
  # Use rlang's .data pronoun for robustness in packages/functions
   if (!requireNamespace("dplyr", quietly = TRUE)) {
       # Basic R approach if dplyr is not available
       deposition_data$deposition <- as.numeric(deposition_data$DepositionLayer)
       deposition_data <- deposition_data[!is.na(deposition_data$deposition) & deposition_data$deposition > 0, ]
       deposition_data$DepositionLayer <- NULL # Remove original string column
   } else {
       # dplyr approach
       deposition_data <- deposition_data %>%
         dplyr::mutate(deposition = as.numeric(.data$DepositionLayer)) %>%
         dplyr::filter(!is.na(.data$deposition) & .data$deposition > 0) %>%
         dplyr::select(-.data$DepositionLayer) # Remove original string column
   }


  # Return the cleaned data frame
  return(as_tibble(deposition_data)) # Ensure tibble output if using tidyverse
}

# Helper function to ensure tibble output if dplyr/tibble loaded
as_tibble <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}