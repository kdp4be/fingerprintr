#include <Rcpp.h>
using namespace Rcpp;

// This is a simple function using Rcpp that creates an R list
// containing a character vector and a numeric vector.
//
// Learn more about how to use Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:
//
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List rcpp_hello() {
  CharacterVector x = CharacterVector::create("foo", "bar");
  NumericVector y   = NumericVector::create(0.0, 1.0);
  List z            = List::create(x, y);
  return z;
}


// [[Rcpp::export]]
NumericVector findmaxgradinregion(int x1, int y1, int areasize, IntegerMatrix &print_info) {
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

// [[Rcpp::export]]
NumericMatrix quality_scores(IntegerMatrix minutiae_info, IntegerMatrix print_info) {

  // ptxy scores minutiae + quality score information (here: assume lqm output)
  // use output from r - matrix called minutiae
  int num_minutiae = minutiae_info.nrow();

  //pix1 stores fingerprint image (as text, 2d array)
  // use output from r

  // initialize 2d vector to place scores in of dimension num_minutiae x 5 (assume lqm output)
  NumericMatrix quality_info(num_minutiae, 5);

  //printf("x\ty\tnewx\tnewy\tqualmetric\n");

  // calculate the output - bigg, contrast, qualmetric
  int newx, newy;
  int areasize = 10;
  for (int p = 0; p < num_minutiae; ++p) {
    newx = minutiae_info(p, 0);
    newy = minutiae_info(p, 1);
    //printf("start at point: %d %d %d\n",newx,newy,pix1[newx][newy]);
    // find the largest grad in the region and move point
    NumericVector bigg = findmaxgradinregion(newx, newy, areasize, print_info);
    float contrast = findcontrast(bigg[1], bigg[2], print_info);
    // delete code used for printing name, can be recovered from other files...
    float qualmetric = bigg[0]*contrast;
    //if (qualmetric > 100.0) qualmetric = 100;
    // add to vector instead of printing
    NumericVector temp = NumericVector::create(newx, newy, bigg[1], bigg[2], qualmetric);
    quality_info(p, _) = temp;
  }

  return(quality_info);
}