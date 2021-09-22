\name{vietnameseConverter-package}
\alias{vietnameseConverter-package}
\alias{vietnameseConverter}

\encoding{UTF-8}

\docType{package}

\title{
The vietnameseConverter package
}
\description{
This package helps you read and use data from Vietnamese sources in R. These data often use Vietnamese legacy character encodings such as TCVN (Vietnam Standards / Tieu chuan Viet Nam) which are still in use today, but are not read correctly in R.

To correct this problem and make such data available in R, this package converts character vectors and entire data frames from legacy Vietnamese encodings to the correct Unicode charactes. It currently supports conversions from three different Vietnamese encodings (TCVN3, VISCII, VPS) to Unicode and vice versa.

The main function is \code{\link{decodeVN}}.
}
\author{
Juergen Niedballa
}
