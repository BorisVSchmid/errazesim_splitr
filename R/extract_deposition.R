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
#'   output ASCII file (e.g., "path/to/output.txt"). The function uses the 
#'   '-s' flag of con2asc, so con2asc might ignore this exact name and create 
#'   a file based on the input name (e.g., output.bin.txt), but this path is used
#'   by the R function to read the expected output. It's recommended to match
#'   the name con2asc will create (e.g., use "path/to/output.bin.txt" if 
#'   input is "path/to/output.bin").
#'
#' @return A data frame (tibble) with columns: YEAR, MO, DA, HR, LAT, LON, 
#'   and Deposition (numeric). Rows with non-positive or NA deposition values 
#'   are filtered out.
#'
#' @importFrom utils read.table
#' @importFrom dplyr mutate filter rename
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' deposition_df <- extract_deposition(
#'   con2asc_path = "C:/hysplit/exec/con2asc.exe",
#'   input_bin_path = "C:/hysplit_runs/run1/output.bin",
#'   output_ascii_path = "C:/hysplit_runs/run1/output.bin.txt"  # This output is determined by con2asc, and is not an option you control.
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
#'                               coords = c("LON", "LAT"), 
#'                               crs = 4326)
#' 
#' ggplot(deposition_sf) +
#'   geom_sf(aes(color = Deposition)) +
#'   scale_color_viridis_c() +
#'   theme_minimal()
#' }
extract_deposition <- function(con2asc_path, 
                                    input_bin_path, 
                                    output_ascii_path) {

  # Check if the output ASCII file already exists
  if (file.exists(output_ascii_path)) {
    warning("ASCII file '", output_ascii_path, "' already exists. Reading existing file.", call. = FALSE)
  } else {
    # Output file does not exist, proceed with conversion
    
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
    # con2asc typically names the output based on input (e.g., input.bin -> input.bin.txt)
    # The -o flag might be ignored when -s is used, check con2asc docs if needed.
    # Ensure paths are quoted for safety.
    command <- paste0('"', con2asc_path, '" -i"', input_bin_path, '" -s')
    
    # Run the command
    # Use system2 for better control/error checking if preferred, but system is simpler
    status <- system(command)
    
    # Basic check for command execution success
    if (status != 0) {
      stop("con2asc command failed with status code: ", status, 
           ". Check HYSPLIT messages or run manually.", call. = FALSE)
    }
    
    # Add a small delay just in case the filesystem needs a moment
    Sys.sleep(0.5) 
    
    # Double-check if the expected output file was created by con2asc -s
    # Note: con2asc with -s usually creates {input_file_name}.txt
    expected_con2asc_output <- sub("\\.[^.]*$", ".bin.txt", input_bin_path, perl=TRUE) # Heuristic
     if (!file.exists(expected_con2asc_output)) {
         # If the heuristic name didn't work, check the user-provided name
         if (!file.exists(output_ascii_path)){
            stop("con2asc command seemed to run (status 0), but the expected output file '", 
             expected_con2asc_output, "' or '", output_ascii_path, "' was not found.", call. = FALSE)
         } else {
            # If the user-provided name exists, use that path for reading
            expected_con2asc_output <- output_ascii_path
         }
     } else {
        # If the heuristic name exists, prefer reading from that path
         output_ascii_path <- expected_con2asc_output
     }

    print(paste("con2asc command executed successfully. Reading:", output_ascii_path))
  }
  
  # Read the resulting ASCII table
  # Use tryCatch for better error handling during file reading
  deposition_data <- tryCatch({
     utils::read.table(output_ascii_path,
                      header = FALSE, 
                      skip = 1, # Skip the header row generated by con2asc
                      col.names = c("YEAR", "MO", "DA", "HR", "LAT", "LON", "DepositionStr"),
                      stringsAsFactors = FALSE, # Prevent factors
                      encoding = "UTF-8", # Specify encoding if needed
                      blank.lines.skip = TRUE, # Skip blank lines if any
                      comment.char = "") # Avoid treating potential '#' as comments
  }, error = function(e) {
    stop("Failed to read the ASCII file '", output_ascii_path, "'. Error: ", e$message, call. = FALSE)
  })

  # Clean the data using dplyr functions (fully qualified)
  # Convert deposition string to numeric, rename, and filter
  deposition_data <- deposition_data %>%
    dplyr::mutate(deposition = as.numeric(.data$DepositionStr)) %>%
    dplyr::filter(!is.na(.data$deposition) & .data$deposition > 0) %>%
    dplyr::select(-.data$DepositionStr) # Remove original string column

  # Return the cleaned data frame
  return(deposition_data)
}