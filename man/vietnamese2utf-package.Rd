\name{vietnamese2utf-package}
\alias{vietnamese2utf-package}
\alias{vietnamese2utf}

\encoding{UTF-8}

\docType{package}

\title{
The vietnamese2utf package
}
\description{
This package helps you read and use data from Vietnamese sources in R. Such data often use Vietnamese legacy character encodings such as TCVN (Vietnam Standards / Tieu chuan Viet Nam) and are still in use today. Such data are not read correctly in R (which doesn't seem to support these Vietnamese encodings).
To correct this problem and make such data available in R, this package converts character vectors and character columns of data frames from legacy Vietnamese encodings to the correct Unicode charactes. The main function is \code{\link{decodeVN}}.
}
\author{
Juergen Niedballa
Maintainer:Juergen Niedballa <niedballa@izw-berlin.de>
}
