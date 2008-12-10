#                               8             o      8               
#                               8                    8               
#  `o  o' ooYoYo. .oPYo. .oPYo. 8oPYo. oPYo. o8 .oPYo8 .oPYo. .oPYo. 
#   `bd'  8' 8  8 .oooo8 8    8 8    8 8  `'  8 8    8 8    8 8oooo8 
#   d'`b  8  8  8 8    8 8    8 8    8 8      8 8    8 8    8 8.     
#  o'  `o 8  8  8 `YooP8 8YooP' `YooP' 8      8 `YooP' `YooP8 `Yooo' 
#                        8                                  8        
#                        8                              YooP'
#
# Author:  Tim Yates and Crispin J Miller
# Date:    2008/08/13
# Licence: LGPL-v3
#
# This file tests all of the exported methods from the library to make sure
# they work as expected (generate the correct files), fail cleanly as required,
# and clean up after themselves in case of faliure after a particular file has
# been created
#

library( xmapbridge )

.test.env <- new.env( hash=TRUE, parent=emptyenv() )

###############################################################################
## Assert methods (to make the code look a bit cleaner)
###############################################################################

assertEqual <- function( a, b, msg="not equal" ) {
  if( class( a ) == "list" && class( b ) == "list" ) {
    if( !isTRUE( all.equal( a, b ) ) ) {
      stop( msg )
    }
  }
  else if( a != b ) {
    stop( msg )
  }
  TRUE
}

assertNotEqual <- function( a, b, msg="equal" ) {
  if( class( a ) == "list" && class( b ) == "list" ) {
    if( isTRUE( all.equal( a, b ) ) ) {
      stop( msg )
    }
  }
  else if( a == b ) {
    stop( msg )
  }
  TRUE
}

assertLength <- function( a, len, msg="invalid length" ) {
  if( length( a ) != len ) {
    stop( msg )
  }
  TRUE
}

assertNull <- function( a, msg="is null" ) {
  if( !is.null( a ) ) {
    stop( msg )
  }
  TRUE
}

assertNotNull <- function( a, msg="is not null" ) {
  if( is.null( a ) ) {
    stop( msg )
  }
  TRUE
}

assertNumeric <- function( a, msg="non-numeric" ) {
  if( !is.number( a ) ) {
    stop( msg )
  }
  TRUE
}

shouldFail <- function( test, msg=paste( "FAIL:", test ) ) {
  .ok <- FALSE
  tryCatch( {
    test()
    .ok <- TRUE
  }, warning=function(e) {
    print( paste( "caught a warning '", e$message, "'" ) ) 
  }, error=function(e) {
    print( paste( "Failed (yay) '", e$message, "'" ) ) 
  } )
  if( .ok ) {
    stop( msg )
  }
  TRUE
}

###############################################################################
## Setup and Teardown Methods
###############################################################################

.setup <- function() {
  .test.env$tmp  <- tempdir()
  .test.env$old  <- Sys.getenv( "XMAP_BRIDGE_CACHE" )
  Sys.setenv( XMAP_BRIDGE_CACHE=.test.env$tmp )
}

.teardown <- function() {
  # Delete the temp folder, and reset our environment
  Sys.setenv( XMAP_BRIDGE_CACHE=.test.env$old )
  unlink( .test.env$tmp, recursive=TRUE )
}

###############################################################################
## And call the tests...
###############################################################################

# Set up the environment
###############################################################################

.setup()

# Project tests
###############################################################################

shouldFail( function() { xmap.project.load( "FAIL!" ) }, "Invalid project parameter shouldn't work!" )
shouldFail( function() { xmap.project.save( "FAIL!", list() ) }, "Invalid project parameter shouldn't work!" )
shouldFail( function() { xmap.project.delete( "FAIL!" ) }, "Invalid project parameter shouldn't work!" )

.project <- xmap.project.new( "Project A" )
.projects <- xmap.project.list()
assertLength( .projects, 1, paste( "Got", length( .projects ), "projects.  Should have only 1" ) )
assertEqual( .project, .projects[[1]], "Project not equal to project in list" )

.data <- xmap.project.load( .project )
assertNotNull( .data, "Load failed for data" )
assertLength( .data, 1, "Data should have 1 element" )
assertEqual( "Project A", .data$NAME )
.data$NAME <- "Project Z"
xmap.project.save( .project, .data )
.data2 <- xmap.project.load( .project )
assertEqual( "Project Z", .data2$NAME, "Did not load altered project name" )
assertEqual( .data, .data2, "Saved and loaded data not identical!" )

.project2 <- xmap.project.new( "Project B" )
.projects <- xmap.project.list()
assertLength( .projects, 2, paste( "Got", length( .projects ), "projects.  Should have 2" ) )

shouldFail( function() { assertEqual( .project, .project2 ) }, paste( "Project 1 and 2 should not be equal" ) )
assertNotEqual( .project, .project2, paste( "Project 1 and 2 should not be equal" ) )

xmap.project.delete( .project2 )

.projects <- xmap.project.list()
assertLength( .projects, 1, paste( "Got", length( .projects ), "projects.  Should have only 1" ) )

# Graph tests
###############################################################################

shouldFail( function() {    xmap.graph.new( "NonProjectObject", "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" ) }, "Invalid project parameter" )
shouldFail( function() {   xmap.graph.load( "FAIL!" )         }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {   xmap.graph.list( "FAIL!" )         }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {   xmap.graph.save( "FAIL!", list() ) }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() { xmap.graph.delete( "FAIL!" )         }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {    xmap.graph.new( .project2, "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" ) }, "Writing to a missing project seeme to work?" )

.graph <- xmap.graph.new( .project, "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" )

.graphs <- xmap.graph.list( .project )
assertLength( .graphs, 1, paste( "Got", length( .graphs ), "graphs.  Should have only 1" ) )
assertEqual( .graph, .graphs[[1]], "Graph not equal to first graph in list" )

.data <- xmap.graph.load( .graph )
assertNotNull( .data, "Load failed for data" )
assertLength( .data, 9, "Graph data should have 9 elements" )
assertEqual( .data$MIN, -10 )
.data$MIN <- 0
xmap.graph.save( .graph, .data )
.data2 <- xmap.graph.load( .graph )
assertEqual( .data2$MIN, 0 )
assertEqual( .data, .data2, "Saved and loaded graph data not identical!" )

.graph2 <- xmap.graph.new( .project, "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" )

.graphs <- xmap.graph.list( .project )
assertLength( .graphs, 2, paste( "Got", length( .graphs ), "graphs.  Should have 2" ) )

xmap.graph.delete( .graph2 )

# Plot tests
###############################################################################

.X <- seq( 1000000, 2000000, by=1000 )
.Y <- runif( length( .X ), -10, 10 )
.badY <- runif( ( length( .X ) - 1 ), -10, 10 )
.alphaY <- rep( letters, ceiling( length( .X ) / length( letters ) ) )[ 1:length( .X ) ]

print( paste( ".X has", length( .X ), "elements, .Y has", length( .Y ), ", .badY has", length( .badY ), "elements, and .alphaY has", length( .alphaY ), "elements" ) )

shouldFail( function() {    xmap.plot.new( "FAIL!", "A Plot", .X, .Y, type="line" ) }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {   xmap.plot.load( "FAIL!" )                 }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {   xmap.plot.list( "FAIL!" )                 }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {   xmap.plot.save( "FAIL!", list(), .X, .Y ) }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() { xmap.plot.delete( "FAIL!" )                 }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {    xmap.plot.new( .graph2, "A Plot", .X, .Y, type="line" ) }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() {    xmap.plot.new( .graph, "A Plot", .X, .badY, type="line" ) }, "Invalid y list size shouldn't work!" )

.plot <- xmap.plot.new( .graph, "A Plot", .X, .Y, type="line" )

.plots <- xmap.plot.list( .graph )
assertLength( .plots, 1, paste( "Got", length( .plots ), "plots.  Should have only 1" ) )
assertEqual( .plot, .plots[[1]], "Plot does not equal the first plot in the list" )

.data <- xmap.plot.load( .plot )
assertNotNull( .data, "Load failed for plot data" )
shouldFail( function() { assertLength( .data, 0 ) }, "Plot data should have more than 0 elements" )
assertEqual( .data$TYPE, "line", "Plot type should be 'line'" )
.data$TYPE <- "area"
xmap.plot.save( .plot, .data, .X, .Y )
.data2 <- xmap.plot.load( .plot )
assertEqual( .data2$TYPE, "area", "Plot type should now be 'area'" )
assertEqual( .data, .data2, "Saved and loaded plot data not identical!" )

.plot2 <- xmap.plot.new( .graph, "A Plot", .X, .Y, type="line" )

.plots <- xmap.plot.list( .graph )
assertLength( .plots, 2, paste( "Got", length( .plots ), "plots.  Should have only 2" ) )

xmap.plot.delete( .plot )
.plots <- xmap.plot.list( .graph )
assertLength( .plots, 1, paste( "Got", length( .plots ), "plots.  Should have only 1" ) )

# Util tests
###############################################################################

shouldFail( function() { xmap.plot( .X, .badY, species="homo_sapiens", "1", type="line" ) }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() { xmap.plot( .X, .alphaY, species="homo_sapiens", "1", type="line" ) }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() { xmap.plot( .X, .Y, species="homo_sapiens", "1", type="INVALID" ) }, "Invalid graph parameter shouldn't work!" )
shouldFail( function() { xmap.plot( .X, .Y, species="MONKEY", "1" ) }, "Invalid species parameter shouldn't work!" )
shouldFail( function() { xmap.plot( .X, .Y, species="homo_sapiens" ) }, "Missing chromosome should have failed!" )
.projects <- xmap.project.list()
assertLength( .projects, 1, paste( "Got", length( .projects ), "projects.  There should still just be 1" ) )

.plot <- xmap.plot( .X, .Y, chr="1" )
.projects <- xmap.project.list()
assertLength( .projects, 2, paste( "Got", length( .projects ), "projects.  There should be 2" ) )

.graphs <- xmap.graph.list( .plot )
assertLength( .graphs, 1, paste( "Got", length( .graphs ), "graphs.  There should be 1" ) )

.graph <- xmap.points( .X, .Y )
.plots <- xmap.plot.list( .plot )
assertLength( .plots, 2, paste( "Got", length( .plots ), "plots.  There should be 2" ) )

xmap.project.delete( .project )
xmap.project.delete( .plot )
.projects <- xmap.project.list()
assertLength( .projects, 0, paste( "Got", length( .projects ), "projects.  There shouldn't be any" ) )

# Cleanup
###############################################################################

.teardown()
