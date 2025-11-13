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
