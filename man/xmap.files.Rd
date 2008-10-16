\name{xmap.files}
\alias{xmap.debug}
\alias{xmap.project.new}
\alias{xmap.project.list}
\alias{xmap.project.load}
\alias{xmap.project.save}
\alias{xmap.project.delete}
\alias{xmap.graph.new}
\alias{xmap.graph.list}
\alias{xmap.graph.load}
\alias{xmap.graph.save}
\alias{xmap.graph.delete}
\alias{xmap.plot.new}
\alias{xmap.plot.list}
\alias{xmap.plot.load}
\alias{xmap.plot.save}
\alias{xmap.plot.delete}
\alias{==,Graph,Graph-method}
\alias{==,Plot,Plot-method}
\alias{==,Project,Project-method}
\alias{!=,Graph,Graph-method}
\alias{!=,Plot,Plot-method}
\alias{!=,Project,Project-method}
\alias{as.character,Graph-method}
\alias{as.character,Plot-method}
\alias{as.character,Project-method}
\title{ 
  Underlying xmap methods for manipulating projects, graphs, and plots
}
\description{
  These methods are used to control xmap.projects, xmap.graphs and xmap.plots

  A project contains one or more graphs, and a graph can contain one or more plots (each plot can have a different style)

  A simpler method to generate X:Map graphs is to use the xmap.plot and xmap.points methods.

  The xmap.debug method was introduced as otherwise the vignette would not print out id values as it was supposed to.  For some reason Sweave ignores the show and as.character methods being overridden, and simply does its own thing.
}
\usage{

  xmap.debug( idobject, newlines=FALSE )

  xmap.project.new( name )
  xmap.project.list()
  xmap.project.load( projectid )
  xmap.project.save( projectid, data )
  xmap.project.delete( projectid )

  xmap.graph.new( projectid, name, desc, min, max, chr, start, stop, ylab="value",
                  species=c("homo_sapiens", "mus_musculus", "rattus_norvegicus") )
  xmap.graph.list( projectid )
  xmap.graph.load( graphid )
  xmap.graph.save( graphid, data )
  xmap.graph.delete( graphid )

  xmap.plot.new( graphid, name, x, y, 
                 type=c("scatter", "line", "bar", "step", "area", "steparea"),
                 col = NULL, dp = 2 )
  xmap.plot.list( graphid )
  xmap.plot.load( plotid )
  xmap.plot.save( plotid, data, x, y, dp = 2 )
  xmap.plot.delete( plotid )

}
\arguments{
  \item{idobject}{ An id object for an xmapbridge project, graph or plot }
  \item{newlines}{ Should the resultant string be nicely formatyted with newline characters? }
  \item{name}{ The name for this object (stored in the NAME variable of the list returned by load) }
  \item{projectid}{ The projectid (returned by xmap.project.new and as a list from xmap.project.list) }
  \item{data}{ This is a list object for this particular object type }
  \item{desc}{ The description for this item }
  \item{min}{ The minimum y value for the graph }
  \item{max}{ The maximum y value for the graph }
  \item{chr}{ A string representing the chromosome this graph is to be drawn on }
  \item{start}{ The BP that this graph should start on }
  \item{stop}{ The BP that this graph reaches until.  Must be greater than start }
  \item{ylab}{ The label for the yaxis of this graph }
  \item{species}{ The species that this graph is to be drawn on }
  \item{graphid}{ The graphid returned by xmap.graph.new or xmap.graph.list }
  \item{type}{ The type of graph you wish to draw }
  \item{col}{ The colour of the graph defined as an integer in the format 0xAARRGGBB.  See \code{\link{xmap.col}} }
  \item{x}{ The points for the x-axis of this plot.  These points must be offsets from the start of the graph, and not their actual chromosomal location }
  \item{y}{ The points for the y-axis of this plot.  Points where min>y or y >max will not be displayed }
  \item{dp}{ The number of decimal places you want to save for the y-coordinates }
  \item{plotid}{ The plotid which is returned by xmap.plot.new or xmap.plot.list }
}
\value{
  xmap.graph.new returns the graphid of the created graph
  xmap.graph.list returns a vector of all graphs contained within the given project
  xmap.graph.load returns a list containing the parameters for the given graph
  xmap.graph.save 
}
\author{ Tim Yates }
\seealso{ 
  \code{\link{xmap.plot}}\cr
  \code{\link{xmap.points}}\cr
  \code{\link{xmap.col}}\cr
  \url{http://xmap.picr.man.ac.uk}
 }
\examples{
  \dontshow{
    path     <- tempdir()
    old.path <- Sys.getenv( "XMAP_BRIDGE_CACHE" )
    Sys.setenv( XMAP_BRIDGE_CACHE=path )
  }
  # Create a new project
  projectid <- xmap.project.new( "A quick project" )

  # Create a graph in this project covering 100K bases on chromosome 1
  graphid <- xmap.graph.new( projectid, "A quick graph", "This is a quick graph", 
                            -10, 10, "1", 5000, 105000, species="homo_sapiens" )

  # Make our datasets
  x <- seq( 1000, 100000, by = 1000 )
  y <- runif( length( x ), -10, 10 )

  # Then add a plot to this graph
  xmap.plot.new( graphid, "Graph 1", x, y, type="area", col=0xAAFFEE00 )

  # And another one, in a different colour  
  a <- xmap.plot.new( graphid, "Graph 2", x, y, type="scatter", col=0xAA0000FF )
  
  cat( xmap.debug( a, newlines=TRUE ) )

  # At this point, we should be able to see the graphs using the XMapBridge application

  # Clean up our project file...
  xmap.project.delete( projectid )
  \dontshow{
    Sys.setenv( XMAP_BRIDGE_CACHE=old.path )
  }
}
