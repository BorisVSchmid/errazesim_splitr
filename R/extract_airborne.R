#' Read HYSPLIT VMSDIST (air-borne vertical mass distribution) output
#'
#' Runs **vmsread.exe** as needed and ingests the resulting ASCII file.
#' Now robust to the case where **vmsread** writes `<input>.txt` instead of an
#' explicitly supplied name, and it keeps `Mass_g` (the block-total mass) per
#' profile.
#'
#' @param vmsread_path  Full path to *vmsread.exe*.
#' @param input_vms_path Full path to binary *VMSDIST*.
#' @param output_ascii_path  Optional.  Desired ASCII path.  When `NULL`
#'   (default), the function expects **vmsread** to create
#'   `paste0(input_vms_path, ".txt")`.
#' @param overwrite Logical. Force re-run of *vmsread* even if the ASCII file
#'   already exists.  Default `FALSE`.
#'
#' @return Tibble with columns
#'   YEAR MO DA HR PROFILE Mass_g HEIGHT_m Incremental Cumulative
#'   (one row per height layer).
#'
#' @export
extract_airborne <- function(vmsread_path,
                             input_vms_path,
                             output_ascii_path = NULL,
                             overwrite = FALSE) {
  
  if (!file.exists(vmsread_path))
    stop("vmsread executable not found at: ", vmsread_path, call. = FALSE)
  if (!file.exists(input_vms_path))
    stop("Input VMSDIST file not found at: ", input_vms_path, call. = FALSE)
  
  # Where vmsread is expected to write if we don’t give -o
  default_ascii_path <- paste0(input_vms_path, ".txt")
  
  # Use caller-supplied path or fall back to the default
  if (is.null(output_ascii_path))
    output_ascii_path <- default_ascii_path
  
  need_run <- overwrite || !file.exists(output_ascii_path)
  
  if (need_run) {
    if (overwrite) {
      suppressWarnings(file.remove(output_ascii_path, default_ascii_path))
    }
    
    message("Converting VMSDIST ➜ ASCII with vmsread (may take a moment)…")
    status <- system(paste0('"', vmsread_path, '" -i"', input_vms_path, '"'))
    
    # vmsread returns 0 even on some failures, so we look for *either* file
    ascii_to_read <- c(output_ascii_path, default_ascii_path)
    ascii_to_read <- ascii_to_read[file.exists(ascii_to_read)][1]
    
    if (is.na(ascii_to_read))
      stop("vmsread finished (status ", status,
           ") but no ASCII file was found at\n  ",
           output_ascii_path, " nor\n  ", default_ascii_path, call. = FALSE)
  } else {
    ascii_to_read <- output_ascii_path
  }
  
  ## ---- parse the ASCII ----------------------------------------------------
  raw <- readLines(ascii_to_read, warn = FALSE)
  raw <- raw[trimws(raw) != ""]                      # drop blanks
  
  is_hdr  <- vapply(strsplit(raw, "\\s+"), length, integer(1)) >= 7
  hdr_idx <- which(is_hdr)
  n_blk   <- length(hdr_idx)
  
  out <- vector("list", n_blk)
  
  for (i in seq_len(n_blk)) {
    s <- hdr_idx[i]
    e <- if (i < n_blk) hdr_idx[i + 1] - 1 else length(raw)
    
    h <- scan(text = raw[s], quiet = TRUE)           # YEAR MO DA HR ID REC MASS
    stub <- data.frame(
      YEAR = h[1], MO = h[2], DA = h[3], HR = h[4],
      PROFILE = i,
      Mass_g  = h[7]
    )
    
    prof <- read.table(text = raw[(s + 1):e],
                       col.names = c("HEIGHT_m", "Incremental", "Cumulative"))
    
    out[[i]] <- cbind(stub[rep(1, nrow(prof)), ], prof)
  }
  
  dplyr::as_tibble(do.call(rbind, out)) %>%
    dplyr::filter(!is.na(Incremental), Incremental > 0)
}
