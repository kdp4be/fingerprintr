#include <Rcpp.h>

using namespace Rcpp;



//' Find the gradient of intensity between the dark and light regions defining a minutiae point.
//'
//' @param x1 X pixel value of a given feature.
//' @param y1 Y pixel value of a given feature.
//' @param print_info Fingerprint image as matrix.
//' @return The maximum gradient within a radius around a given point and the shifted X and Y pixel location values of the pixel in a small neighborhood of the minutiae producing the highest intensity gradient values.
//' @examples
//' print_image <- convert_image(G080_image, "bmp")
//' findmaxgradinregion(841, 541, print_image)
//' @export
// [[Rcpp::export]]
NumericVector findmaxgradinregion(int x1, int y1, int areasize, IntegerMatrix print_info) {
  // find max grads at each point
  float bigg = 0.0;
  int newx = x1;
  int newy = y1;
  float gg, dist, dx, dy;
  // find the largest gradient in this 5 x 5 area
  for (int j = y1 - areasize; j < y1+areasize; ++j) {
    // compare this row to everything larger
    for (int k = x1-areasize; k < x1+areasize; ++k) {
      // compare this column to everything larger
      // find gradient between k,j and everything larger
      for (int jj = j+1; jj < y1+areasize; ++jj) {
        for (int kk = k+1; kk < x1+areasize; ++kk) {
          dx = (float)(k-kk);
          dy = (float)(j-jj);
          dist = sqrt(dx*dx + dy*dy);
          gg = (float)(print_info(kk, jj) - print_info(k, j));
          gg = gg/dist;
          if (gg > bigg) {
            bigg = gg;
            // pick the darker pixel
            if (print_info(k, j) < print_info(kk, jj)) {
              newx = k;
              newy = j;
            }
            else {
              newx = kk;
              newy = jj;
            }
          }
        }
      }
    }
  }
  NumericVector temp = NumericVector::create(bigg, newx, newy);
  return(temp);
}



//' Find an overall contrast of intensity values in the general neighborhood of a minutiae.
//'
//' @param kk X pixel value of a given feature.
//' @param yy Y pixel value of a given feature.
//' @param print_info Fingerprint image as matrix.
//' @return The largest intensity difference between a point and any neighbor intensity in a 3x3 region, divided by the intensity in the print (usually 255).
//' @examples
//' print_image <- convert_image(G080_image, "bmp")
//' findcontrast(831, 542, print_image)
//' @export
// [[Rcpp::export]]
float findcontrast(int kk, int jj, IntegerMatrix &print_info) {
  int area = 3;
  int bigi = 0;
  int ii;
  for (int j = jj-area; j < jj+area; ++j) {
    for (int k = kk-area; k < kk+area; ++k) {
      ii = abs(print_info(kk, jj) - print_info(k, j));
      if (ii > bigi)
        bigi = ii;
    }
  }
  return((float)(bigi)/255.0);
}



//' Find the quality scores of individual minutiae.
//'
//' @param print_info Fingerprint image as matrix.
//' @param minutiae_info Feature (minutiae) information as matrix
//' @param verbose If TRUE, will output gradient and contrast information in addition to the quality score.
//' @return Matrix containing the columns: x pixel location of minutiae, y pixel location of minutiae, shifted x location, shifted y location, quality score.
//' @examples
//' data(G080_image); data(G080_min)
//' print_image <- convert_image(G080_image, "bmp")
//' min_info <- import_features(G008_min)
//' quality_scores(print_image, min_info)
//' @export
// [[Rcpp::export]]
NumericMatrix quality_scores(IntegerMatrix print_info, IntegerMatrix minutiae_info,
  bool verbose = false) {

  // get number of minutiae
  int num_minutiae = minutiae_info.nrow();

  // initialize output matrix info
  int quality_dim = 5;
  CharacterVector col_names = CharacterVector::create("X", "Y", "Shifted X", "Shifted Y", "Quality Score");
  if (verbose) {
    quality_dim = 7;
    col_names = CharacterVector::create("X", "Y", "Shifted X", "Shifted Y",
      "Max gradient in region", "Contrast", "Quality Score");
  }
  NumericMatrix quality_info(num_minutiae, quality_dim);

  // calculate QM
  int newx, newy;
  int areasize = 10;
  for (int p = 0; p < num_minutiae; ++p) {
    newx = minutiae_info(p, 0);
    newy = minutiae_info(p, 1);
    // find the largest grad in the region and move point
    NumericVector bigg = findmaxgradinregion(newx, newy, areasize, print_info);
    float contrast = findcontrast(bigg[1], bigg[2], print_info);
    float qualmetric = bigg[0]*contrast;
    if (qualmetric > 100.0)
      qualmetric = 100.0;
    // save quality info
    NumericVector temp = NumericVector::create(newx, newy, bigg[1], bigg[2], qualmetric);
    if (verbose) {
      temp = NumericVector::create(newx, newy, bigg[1], bigg[2], qualmetric, bigg[0], contrast);
    }
    quality_info(p, _) = temp;
  }
  colnames(quality_info) = col_names;
  return(quality_info);
}
