#' Read HYSPLIT VMSDIST (vertical mass-distribution) ASCII output
#'
#' Converts a binary *VMSDIST* file to ASCII using **vmsread.exe** (if needed)
#' and parses the resulting *vmsdist.txt* into a tidy data frame.
#'
#' Each block in *vmsdist.txt* begins with seven numbers
#' – `YEAR MO DA HR ID REC TOTAL` – followed by one or more lines that hold the
#' vertical profile for that hour (`HEIGHT_m  INCR_%  CUM_%`).  
#' The helper below keeps those pieces together for easy downstream use.
#'
#' @param vmsread_path Full path to **vmsread.exe**.
#' @param input_vms_path Full path to the binary *VMSDIST* file.
#' @param output_ascii_path Desired path for the ASCII file (e.g. “…/vmsdist.txt”).
#'        Leave it at the default you’d expect **vmsread.exe** to create if you
#'        prefer.
#' @param overwrite Logical.  If `TRUE`, re-runs **vmsread** even when the ASCII
#'        file already exists.  Defaults to `FALSE`.
#'
#' @return A tibble with one row per height level, columns
#'   \itemize{
#'     \item `YEAR, MO, DA, HR` – timestamp (UTC)
#'     \item `PROFILE` – running counter (1 = first block in file)
#'     \item `HEIGHT_m` – centre height of the layer (m AGL)
#'     \item `Incremental` – incremental % of the total mass in this layer
#'     \item `Cumulative` – cumulative % that has settled \emph{above} this level
#'   }
#'   All rows with missing or non-positive incremental values are discarded.
#'
#' @examples
#' \dontrun{
#' airborne_df <- extract_airborne(
#'     vmsread_path      = "C:/hysplit/exec/vmsread.exe",
#'     input_vms_path    = "C:/hysplit_runs/run1/VMSDIST",
#'     output_ascii_path = "C:/hysplit_runs/run1/vmsdist.txt"
#' )
#' head(airborne_df)
#' }
#' @export
extract_airborne <- function(vmsread_path,
                             input_vms_path,
                             output_ascii_path,
                             overwrite = FALSE) {
  
  #--- run vmsread.exe when needed -------------------------------------------
  if (!file.exists(output_ascii_path) || overwrite) {
    
    if (!file.exists(vmsread_path))
      stop("vmsread executable not found at: ", vmsread_path, call. = FALSE)
    
    if (!file.exists(input_vms_path))
      stop("Input VMSDIST file not found at: ", input_vms_path, call. = FALSE)
    
    message("Converting VMSDIST ➜ ASCII with vmsread (may take a moment)…")
    
    cmd <- paste0('"', vmsread_path, '" -i"', input_vms_path, '"')
    status <- system(cmd)
    
    if (status != 0 || !file.exists(output_ascii_path))
      stop("vmsread failed (status ", status, "); check input and paths.",
           call. = FALSE)
  }
  
  #--- parse the ASCII file ---------------------------------------------------
  raw <- readLines(output_ascii_path, warn = FALSE)
  raw <- raw[trimws(raw) != ""]          # drop blank lines
  
  is_header <- vapply(strsplit(raw, "\\s+"), length, integer(1)) >= 7
  hdr_idx   <- which(is_header)
  n_blocks  <- length(hdr_idx)
  
  out <- vector("list", n_blocks)
  
  for (i in seq_len(n_blocks)) {
    start   <- hdr_idx[i]
    end     <- if (i < n_blocks) hdr_idx[i + 1] - 1 else length(raw)
    
    # header : YEAR MO DA HR ID REC TOTAL
    h <- scan(text = raw[start], quiet = TRUE)
    prof <- data.frame(
      YEAR = h[1], MO = h[2], DA = h[3], HR = h[4],
      PROFILE = i
    )
    
    # profile rows : HEIGHT  INCR  CUM
    prof_rows <- read.table(text = raw[(start + 1):end],
                            col.names = c("HEIGHT_m", "Incremental", "Cumulative"))
    out[[i]] <- cbind(prof[rep(1, nrow(prof_rows)), ], prof_rows)
  }
  
  dplyr::as_tibble(do.call(rbind, out)) %>%
    dplyr::filter(!is.na(Incremental), Incremental > 0)
}
