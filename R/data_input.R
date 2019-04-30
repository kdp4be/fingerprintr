# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# We would like to thank Dr. Adele Peskin for vital discussion and conversation 
# leading to the creation and development of the Contrast Gradient Algorithm 
# as well as an unpublished manuscript.



#' Process an image file to text.
#'
#' @param image_file A image file of JPG, BMP, or TIFF format imported using readJPG or similar.
#' @param image_format Image format as a string.
#' @return A processed numeric matrix containing image information.
#' @examples
#' G080_image <- read.bmp("data/G080_image.bmp")
#' convert_image(G080_image, "bmp")
convert_image <- function(image_file, image_format) {
	image_dim <- dim(image_file)
	# based on image format, either need values as is or * 255
	if (image_format %in% c("jpeg", "jpg", "tiff"))
		image_data <- image_file*255
	else if (image_format == "bmp")
		image_data <- image_file
	else
		stop("Image format currently unsupported")

	# return text file
	if (length(image_dim) == 2)
		image_as_text <- image_data
	else if (image_dim[3] == 2)
		image_as_text <- image_data[,,1]
	else
		stop("Error: unknown dimensions")

	return(t(as.matrix(image_as_text)))
}



#' Process an file of features (minutiae), assuming x and y pixel locations are contained in the first two columns.
#'
#' @param feature_file Matrix or data frame containing feature information.
#' @return A processed numeric matrix containing the x and y pixel locations of each minutiae.
#' @examples
#' data(G080_min)
#' import_features(G080_min)
import_features <- function(feature_file) {
	# assume feature x, y are in first and second columns
	# wrapper function; currently can just use a data matrix with minutiae values. 
		# however, if assuming LQM input, then the x and y locations are the second and third columns
	features <- feature_file[,c(1,2)]
	features <- as.matrix(features)
	return(features)
}

