#                               8             o      8               
#                               8                    8               
#  `o  o' ooYoYo. .oPYo. .oPYo. 8oPYo. oPYo. o8 .oPYo8 .oPYo. .oPYo. 
#   `bd'  8' 8  8 .oooo8 8    8 8    8 8  `'  8 8    8 8    8 8oooo8 
#   d'`b  8  8  8 8    8 8    8 8    8 8      8 8    8 8    8 8.     
#  o'  `o 8  8  8 `YooP8 8YooP' `YooP' 8      8 `YooP' `YooP8 `Yooo' 
#                        8                                  8        
#                        8                              YooP'
#
# Author:  Tim Yates
# Date:    2008/12/04
# Licence: GPL-3
#
# The is an RUnit implementation of my tests, as it should be an easy win to use
# their functions rather than the ones I wrote in the original tests
#

if(FALSE) {
  library( "RUnit" )
  library( "xmapbridge" )
}

###############################################################################
## Create a test environment
##
.test.env <- new.env( hash=TRUE, parent=emptyenv() )

###############################################################################
## SetUp is called before each test method
##
.setUp <- function() {
  .test.env$tmp  <- tempdir()
  if( !file.exists( .test.env$tmp ) ) dir.create( .test.env$tmp )
  .test.env$old  <- Sys.getenv( "XMAP_BRIDGE_CACHE" )
  Sys.setenv( XMAP_BRIDGE_CACHE=.test.env$tmp )
}

###############################################################################
## TearDown is called after each test method
##
.tearDown <- function() {
  # Delete the temp folder, and reset our environment
  Sys.setenv( XMAP_BRIDGE_CACHE=.test.env$old )
  unlink( .test.env$tmp, recursive=TRUE )
}

##
## Check we can get the cache dir properly
##
test.get.xmap.cache.dir <- function() {
  checkEquals( .test.env$tmp, xmapbridge:::.get.xmap.cache.dir(), "Cache dir doesn't seem to have been set correctly" )
}

###############################################################################
## Check all the incorrect values for x and y fail, and good values pass
##
test.validate.x.and.y <- function() {
  checkException( xmapbridge:::.validate.x.and.y( y=c(1,2,3) ),             "X missing should fail",         silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,'Tim',3), c(1,2,3) ), "X cannot contain non-numerics", silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,NA,3), c(1,2,3) ),    "X contains NA should fail",     silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,NaN,3), c(1,2,3) ),   "X contains NaN should fail",    silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,4/0,3), c(1,2,3) ),   "X contains Inf should fail",    silent=TRUE )

  checkException( xmapbridge:::.validate.x.and.y( x=c(1,2,3) ),              "Y missing should fail",         silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,2,3), c(1,c(1,2),3) ), "Y cannot contain non-numerics", silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,2,3), c(1,NA,3) ),     "Y contains NA should fail",     silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,2,3), c(1,NaN,3) ),    "Y contains NaN should fail",    silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,2,3), c(1,4/0,3) ),    "Y contains Inf should fail",    silent=TRUE )

  checkException( xmapbridge:::.validate.x.and.y( c(1,2,3), c(1,2) ),        "X longer than Y should fail",       silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,2), c(1,2,3) ),        "Y longer than X should fail",       silent=TRUE )
  checkException( xmapbridge:::.validate.x.and.y( c(1,4*NULL,3), c(1,2,3) ), "X contains numeric(0) should fail", silent=TRUE ) # fails due to length
  checkException( xmapbridge:::.validate.x.and.y( c(1,2,3), c(1,4*NULL,3) ), "Y contains numeric(0) should fail", silent=TRUE ) # fails due to length

  ## These two should run ok
  xmapbridge:::.validate.x.and.y( c(1,2,3), c(1,2,3) )
  xmapbridge:::.validate.x.and.y( list(1,2,3), c(1,2,3) )
}

###############################################################################
## Test project creation, updates, deletion, and listing
##
test.projects <- function() {
  checkException( xmap.project.load( "FAIL!" ),         "Invalid project parameter shouldn't work!", silent=TRUE )
  checkException( xmap.project.save( "FAIL!", list() ), "Invalid project parameter shouldn't work!", silent=TRUE )
  checkException( xmap.project.delete( "FAIL!" ),       "Invalid project parameter shouldn't work!", silent=TRUE )
  .project <- xmap.project.new( "Project A" )
  .projects <- xmap.project.list()
  checkEqualsNumeric( length( .projects ), 1, paste( "Got", length( .projects ), "projects.  Should have only 1" ) )
  checkIdentical( .project, .projects[[1]], "Project not equal to project in list" )
  
  .data <- xmap.project.load( .project )
  checkEqualsNumeric( length( .data ), 1, "Data should have 1 element" )
  checkEquals( "Project A", .data$NAME )
  .data$NAME <- "Project Z"
  xmap.project.save( .project, .data )
  .data2 <- xmap.project.load( .project )
  checkEquals( "Project Z", .data2$NAME, "Did not load altered project name" )
  checkEquals( .data, .data2, "Saved and loaded data not identical!" )

  .project2 <- xmap.project.new( "Project B" )
  .projects <- xmap.project.list()
  checkEqualsNumeric( length( .projects ), 2, paste( "Got", length( .projects ), "projects.  Should have 2" ) )

  xmap.project.delete( .project2 )

  .projects <- xmap.project.list()
  checkEqualsNumeric( length( .projects ), 1, paste( "Got", length( .projects ), "projects.  Should have only 1" ) )
}

###############################################################################
## Test graph creation, updates, deletion, and listing
##
test.graph <- function() {
  .project <- xmap.project.new( "Project A" )
  checkException( xmap.graph.new( "NonProjectObject", "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" ), "Invalid project parameter", silent=TRUE )
  checkException( xmap.graph.load( "FAIL!" ),         "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.graph.list( "FAIL!" ),         "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.graph.save( "FAIL!", list() ), "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.graph.delete( "FAIL!" ),       "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.graph.new( .project2, "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" ), "Writing to a missing project seeme to work?", silent=TRUE )
 
  .graph <- xmap.graph.new( .project, "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" )
  .graphs <- xmap.graph.list( .project )
  checkEqualsNumeric( length( .graphs ), 1, paste( "Got", length( .graphs ), "graphs.  Should have only 1" ) )
  checkIdentical( .graph, .graphs[[1]], "Graph not equal to first graph in list" )

  .data <- xmap.graph.load( .graph )
  checkEqualsNumeric( length( .data ), 9, "Graph data should have 9 elements" )
  checkEqualsNumeric( .data$MIN, -10,     "Minimum should be -10" )

  .data$MIN <- 0
  xmap.graph.save( .graph, .data )

  .data2 <- xmap.graph.load( .graph )
  checkEqualsNumeric( .data2$MIN, 0 )
  checkEquals( .data, .data2, "Saved and loaded graph data not identical!" )

  .graph2 <- xmap.graph.new( .project, "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" )
  .graphs <- xmap.graph.list( .project )
  checkEqualsNumeric( length( .graphs ), 2, paste( "Got", length( .graphs ), "graphs.  Should have 2" ) )
  xmap.graph.delete( .graph2 )
  .graphs <- xmap.graph.list( .project )
  checkEqualsNumeric( length( .graphs ), 1, paste( "Got", length( .graphs ), "graphs.  Should have 1" ) )
}

###############################################################################
## Test plot creation, updates, deletion, and listing
##
test.plots <- function() {
  .project <- xmap.project.new( "Project A" )
  .graph <- xmap.graph.new( .project, "A Graph", "desc", -10, 10, "1", 1000000, 2000000, ylab="label", species="homo_sapiens" )

  .X <- seq( 1000000, 2000000, by=1000 )
  .Y <- runif( length( .X ), -10, 10 )
  .badY <- runif( ( length( .X ) - 1 ), -10, 10 )
  .alphaY <- rep( letters, ceiling( length( .X ) / length( letters ) ) )[ 1:length( .X ) ]

  checkException( xmap.plot.new( "FAIL!", "A Plot", .X, .Y, type="line" ),   "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.plot.load( "FAIL!" ),                                 "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.plot.list( "FAIL!" ),                                 "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.plot.save( "FAIL!", list(), .X, .Y ),                 "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.plot.delete( "FAIL!" ),                               "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.plot.new( .graph2, "A Plot", .X, .Y, type="line" ),   "Invalid graph parameter shouldn't work!", silent=TRUE )
  checkException( xmap.plot.new( .graph, "A Plot", .X, .badY, type="line" ), "Invalid y list size shouldn't work!", silent=TRUE )

  .plot <- xmap.plot.new( .graph, "A Plot", .X, .Y, type="line" )
  .plots <- xmap.plot.list( .graph )

  checkEqualsNumeric( length( .plots ), 1, paste( "Got", length( .plots ), "plots.  Should have only 1" ) )
  checkIdentical( .plot, .plots[[1]], "Plot does not equal the first plot in the list" )

  .data <- xmap.plot.load( .plot )
  checkEqualsNumeric( length( .data ), 2, paste( "Plot data has", length( .data ), "elements.  Expected 2" ) )
  checkEquals( .data$TYPE, "line", "Plot type should be 'line'" )

  .data$TYPE <- "area"
  xmap.plot.save( .plot, .data, .X, .Y )
  .data2 <- xmap.plot.load( .plot )
  checkEquals( .data2$TYPE, "area", "Plot type should now be 'area'" )
  checkEquals( .data, .data2, "Saved and loaded plot data not identical!" )

  .plot2 <- xmap.plot.new( .graph, "A Plot", .X, .Y, type="line" )
  .plots <- xmap.plot.list( .graph )
  checkEqualsNumeric( length( .plots ), 2, paste( "Got", length( .plots ), "plots.  Should have only 2" ) )

  xmap.plot.delete( .plot )
  .plots <- xmap.plot.list( .graph )
  checkEqualsNumeric( length( .plots ), 1, paste( "Got", length( .plots ), "plots.  Should have only 1" ) )
}

###############################################################################
## Test the plot and points methods
##
test.utils <- function() {
  ## List the old files in the cache folder
  .old.cache <- list.files( .test.env$tmp )

  .X <- seq( 1000000, 2000000, by=1000 )
  .Y <- runif( length( .X ), -10, 10 )
  .badY <- runif( ( length( .X ) - 1 ), -10, 10 )
  .alphaY <- rep( letters, ceiling( length( .X ) / length( letters ) ) )[ 1:length( .X ) ]
  checkException( xmap.plot( .X, .badY, species="homo_sapiens", "1", 
                             range( 1000000, 2000000 ), range( -10, 10 ), 
                             "A plot", "Sub headline", type="line", xlab="value" ), "bad Y plot value should fail", silent=TRUE )

  # check there are no new files created...
  .new.cache <- list.files( .test.env$tmp )
  checkEquals( .old.cache, .new.cache, "Cache folder seems to have changed..." )

  .project <- xmap.project.new( "Project A" )
  .old.cache <- list.files( .project@project ) ## Cannot call S4 method "xmap.project.folder" *sigh*

  checkException( xmap.plot( .X, .badY, species="homo_sapiens", "1", 
                             range( 1000000, 2000000 ), range( -10, 10 ), 
                             "A plot", "Sub headline", type="line", xlab="value", projectid=.project ), "bad Y plot value should fail", silent=TRUE )

  .new.cache <- list.files( .project@project ) ## Cannot call S4 method "xmap.project.folder" *sigh*
  checkEquals( .old.cache, .new.cache, "Project folder seems to have changed..." )

  .plot <- xmap.plot( .X, .Y, species="homo_sapiens", "1", 
                      range( 1000000, 2000000 ), range( -10, 10 ), 
                      "A plot", "Sub headline", type="line", xlab="value", projectid=.project )
  
  .old.cache <- list.files( file.path( .plot@project, .plot@graph ) )

  checkException( xmap.points( .X, .badY, type="line", xlab="value" ), "Bad Y should crash xmap.points", silent=TRUE )

  .new.cache <- list.files( file.path( .plot@project, .plot@graph ) )
  checkEquals( .old.cache, .new.cache, "Graph folder seems to have changed..." )

  xmap.points( .X, .Y, type="line", xlab="value" )
  .new.cache <- list.files( file.path( .plot@project, .plot@graph ) )
  checkEqualsNumeric( length( .old.cache ) + 1, length( .new.cache ), "Can't find a new plot..." )
}

