\docType{data}
\name{vn_samples}
\alias{vn_samples}
\title{Sample data frames in legacy Vietnamese encodings}
\format{A list with 4 data frames}
\usage{
data(vn_samples)
}
\description{
A list with four data frames. The data frames list the provinces of Viet Nam.

The first list item ($Unicode) shows the correct entries. The other three data frames show what loading data encoded in three different Vietnamese encodings would look like when loaded in R.

The list items are:
\itemize{
\item \bold{Unicode} - Data frame with correct Unicode characters (reference)
\item \bold{TCVN3} -  Data frame with TCVN3-encoded characters
\item \bold{VISCII} -  Data frame with VISCII-encoded characters
\item \bold{VPS} -  Data frame with VPS-encoded characters
}

Note that the last 3 are not actually encoded in their respective Vietnamese encodings. Instead, they show what a table in those encodings would look like when loaded into R (or more generally, a system that is not aware of the encodings).

}
\details{

Each data frame contains 5 colums and 63 rows. The first two are character, the last three numeric.

\itemize{
 \item \bold{Province_city}        -   Name of province
 \item \bold{Administrative_center} -   Administrative center of the province
 \item \bold{Area_km2}             -   Area in km^2
 \item \bold{Density_perkm2}       -   Population density (km^-2)
 \item \bold{HDI_2012}             -   Human development index in 2012
 }

The first two columns are character, the last three numeric. Only the character columns will be modified by calling \code{\link{decodeVN}}, while the numeric columns will not be changed.

Factors are not converted. If your data frame contains factors, convert these to character first.
}

\note{
The data frame is based on the table of provinces of Viet nam on Wikipedia \url{https://en.wikipedia.org/wiki/Provinces_of_Vietnam} with minor edits. The legacy Vietnamese encodings were simulated using the \code{\link{decodeVN}} function and checked with this online conversion tool: \url{http://www.enderminh.com/minh/vnconversions.aspx}.


}
\keyword{datasets}

