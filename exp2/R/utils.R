onepx_in_visd <- function(screen_distance_cm, screen_dpi) { # If you do not know the dpi, see: https://dpi.lv
  
  # Convert pixel to centimeter (Remark: 1 inch = 2.54 cm)
  numberofpixel_per_cm <- screen_dpi / 2.54 # The screen has n pixel per inch. 
                                            # If we divide it by 2.54, we know how many pixel the screen has per cm.
  onepixel_in_cm <- 1 / numberofpixel_per_cm # One cm has n pixel. We use Dreisatz to know how long one pixel is in cm.
  
  # Calculate the viewing angle in radians for one pixel
  # Formula from: https://rechneronline.de/sehwinkel/
  onepixel_in_rad <- 2 * atan(onepixel_in_cm / (2 * screen_distance_cm))
  
  # Convert radians for one pixel to degrees
  # Formula from: https://www.rapidtables.com/convert/number/radians-to-degrees.html 
  onepixel_in_visd <- round(onepixel_in_rad * (180 / pi), 5)
  
  return(onepixel_in_visd)
}

#' Read and combine data-quality files from a folder
#'
#' Reads all tab-separated data-quality files from a given preprocessing
#' folder, adds metadata columns, and row-binds them into a single data frame.
#'
#' @param folder Character scalar. Name of the subfolder inside
#'   `exp2/data/preproc_dataquality/` containing the files to be read
#'   (e.g., "alex_calibration_5p").
#'
#' @return A data frame containing all rows from all files in the folder,
#'   with two additional columns:
#'   \itemize{
#'     \item \code{age_group}: the folder name (used as a grouping variable)
#'     \item \code{filename}: the source file name for each row
#'   }
#'
#' @details
#' All files are assumed to be tab-separated (\code{sep = "\\t"}) and to share
#' an identical column structure. Files are read sequentially and combined
#' using \code{dplyr::bind_rows()} via \code{purrr::map_dfr()}.
#'
#' @examples
#' \dontrun{
#' df_alex_5p  <- read_dataquality_folder("alex_calibration_5p")
#' df_human_9p <- read_dataquality_folder("human_calibration_9p")
#' df_ape_2p   <- read_dataquality_folder("ape_calibration_2p")
#' }
read_dataquality_folder <- function(folder){
  
  # List all files in the specified preprocessing folder
  filenames <- list.files(
    path = here("exp2", "data", "preproc_dataquality", folder)
  )
  
  # Read each file and row-bind them into one data frame
  purrr::map_dfr(filenames, \(filename){
    
    read.table(
      here("exp2", "data", "preproc_dataquality", folder, filename),
      header = TRUE,
      sep = "\t"
    ) |>
      mutate(
        condition = folder,  # encode folder as calibration condition
        filename  = filename # keep track of source file
      )
  })
}

