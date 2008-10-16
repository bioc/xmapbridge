\name{xmap.col}
\alias{xmap.col}
\title{
  Generate a colour integer for the xmapBridge
}
\description{
   Convert an R colour into the form 0xAARRGGBB as used by the xmapBridge
}
\usage{

   xmap.col(col,alpha)

}
\arguments{
\item{col}{ The R colour to convert }
\item{alpha}{ Change the colour's alpha value to the one specified }
}
\details{
 Takes one or more hex strings, possibly with an alpha value, as used by R and
converts them to an integer of the form 0xAARRGGBB to be used by the xmapBridge.
If alpha is specified, then change the colours' alpha values to this, otherwise
leave them as they were.
}
\seealso{
  \code{\link{xmap.plot}}\cr
  \code{\link{xmap.plot.new}}
}
\author{
  Crispin J Miller
}
\examples{
  require( RColorBrewer )
  transparent.greens <- xmap.col( brewer.pal( 5, "Greens" ), alpha=0x22 )
}
