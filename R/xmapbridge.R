#                               8             o      8               
#                               8                    8               
#  `o  o' ooYoYo. .oPYo. .oPYo. 8oPYo. oPYo. o8 .oPYo8 .oPYo. .oPYo. 
#   `bd'  8' 8  8 .oooo8 8    8 8    8 8  `'  8 8    8 8    8 8oooo8 
#   d'`b  8  8  8 8    8 8    8 8    8 8      8 8    8 8    8 8.     
#  o'  `o 8  8  8 `YooP8 8YooP' `YooP' 8      8 `YooP' `YooP8 `Yooo' 
#                        8                                  8        
#                        8                              YooP'
#
# R library for outputting graphs in the file structure required by
# the XMapBridge java application so that they can be displayed on
# top of the X:Map genome browser.
#
# Author:  Tim Yates and Crispin J Miller
# Date:    2008/08/13
# Licence: GPL-3

###############################################################################
## Our id classes
###############################################################################

setClass( "Project", 
          representation=representation( project="character" ),
          prototype( project="" ),
          validity=function( object ) { 
            .ret <- TRUE
            if( !.xmap.project.valid( object@project ) )
              .ret <- paste( object@project, "is not a valid project folder" )
            .ret
          } )
setClass( "Graph",
          contains="Project",
          representation=representation( graph="character", minx="numeric" ),
          prototype( graph="", minx=-1 ),
          validity=function( object ) { 
            .ret <- TRUE
            .path <- file.path( object@project, object@graph )
            if( !.xmap.graph.valid( .path ) )
              .ret <- paste( .path, "is not a valid graph folder" )
            if( object@minx < 0 )
              .ret <- paste( "'minx' must be a positive number" )
            .ret
          } )
setClass( "Plot",
          contains="Graph",
          representation=representation( plot="character" ),
          prototype( plot="" ),
          validity=function( object ) { 
            .ret <- TRUE
            .path <- file.path( object@project, object@graph, object@plot )
            if( !file.exists( .path ) )
              .ret <- paste( .path, "is not a valid plot file" )
            .ret
          } )

###############################################################################
## Our id methods
###############################################################################

xmap.debug <- function( idobject, newlines=FALSE ) {
  .ret <- "UNKNOWN"
  if( inherits( idobject, "Project" ) ) {
    .ret <- paste( "xmapbridge:Project(", idobject@project, ")" )
    if( newlines ) {
      .ret <- paste( .ret, "\n" )
    }
  }
  else if( inherits( idobject, "Graph" ) ) {
    .pref <- paste( "xmapbridge:Graph(", idobject@graph, "@", paste( format( idobject@minx, scientific=FALSE ), "bp", sep="", collapse="" ), ")" )
    .suff <- xmap.debug( as( idobject, "Project" ), newlines=newlines )
    if( newlines ) {
      .ret <- paste( .pref, "\n  in ->", .suff )
    }
    else {
      .ret <- paste( .pref, "in", .suff )
    }
  }
  else if( inherits( idobject, "Plot" ) ) {
    .pref <- paste( "xmapbridge:Plot(", idobject@plot, ")" )
    .suff <- xmap.debug( as( idobject, "Graph" ), newlines=newlines )
    if( newlines ) {
      .ret <- paste( .pref, "\n  in ->", .suff )
    }
    else {
      .ret <- paste( .pref, "in", .suff )
    }
  }
  .ret
}

setMethod( "==", c( "Project", "Project" ), function( e1, e2 ) {   e1@project == e2@project } )
setMethod( "==", c( "Graph", "Graph" ),     function( e1, e2 ) { ( e1@project == e2@project ) && ( e1@graph == e2@graph ) && ( e1@minx == e2@minx ) } )
setMethod( "==", c( "Plot", "Plot" ),       function( e1, e2 ) { ( e1@project == e2@project ) && ( e1@graph == e2@graph ) && ( e1@minx == e2@minx ) && ( e1@plot == e2@plot ) } )
setMethod( "!=", c( "Project", "Project" ), function( e1, e2 ) {   e1@project != e2@project } )
setMethod( "!=", c( "Graph", "Graph" ),     function( e1, e2 ) { ( e1@project != e2@project ) || ( e1@graph != e2@graph ) || ( e1@minx != e2@minx ) } )
setMethod( "!=", c( "Plot", "Plot" ),       function( e1, e2 ) { ( e1@project != e2@project ) || ( e1@graph != e2@graph ) || ( e1@minx != e2@minx ) || ( e1@plot != e2@plot ) } )

setGeneric( "show", function( object ) standardGeneric( "show" ) )
setMethod( "show", "Project", function( object ) { cat( xmap.debug( object, newlines=TRUE ) ) } )
setMethod( "show", "Graph",   function( object ) { cat( xmap.debug( object, newlines=TRUE ) ) } )
setMethod( "show", "Plot",    function( object ) { cat( xmap.debug( object, newlines=TRUE ) ) } )

setMethod( "as.character", "Project", function( x, ... ) { show( x ) } )
setMethod( "as.character", "Graph", function( x, ... ) { show( x ) } )
setMethod( "as.character", "Plot", function( x, ... ) { show( x ) } )

setGeneric( "xmap.project.folder", function( object, ... ) standardGeneric( "xmap.project.folder" ) ) ;
setMethod( "xmap.project.folder", "Project", function( object, ... ) { object@project } )
setMethod( "xmap.project.folder", "Graph",   function( object, ... ) { object@project } )
setMethod( "xmap.project.folder", "Plot",    function( object, ... ) { object@project } )

setGeneric( "xmap.graph.folder", function( object, ... ) standardGeneric( "xmap.graph.folder" ) ) ;
setMethod( "xmap.graph.folder", "Graph", function( object, ... ) { file.path( object@project, object@graph ) } )
setMethod( "xmap.graph.folder", "Plot",  function( object, ... ) { file.path( object@project, object@graph ) } )

setGeneric( "xmap.plot.file", function( object, ... ) standardGeneric( "xmap.plot.file" ) ) ;
setMethod( "xmap.plot.file", "Plot", function( object, ... ) { file.path( object@project, object@graph, object@plot ) } )

setGeneric( "xmap.graph.minx", function( object, ... ) standardGeneric( "xmap.graph.minx" ) ) ;
setMethod( "xmap.graph.minx", "Graph", function( object, ... ) { object@minx } )
setMethod( "xmap.graph.minx", "Plot",  function( object, ... ) { object@minx } )

###############################################################################
## Private Utility Methods
###############################################################################

.get.xmap.cache.dir <- function() {
  d <- Sys.getenv( "XMAP_BRIDGE_CACHE" )[[ 1 ]]
  if( d == "" ) {
    # Check for .xmb_cache folder in USER.HOME
    d <- file.path( path.expand( "~" ), ".xmb_cache" )
    if( !file.exists( d ) ) {
      .create <- readline( paste( "Cache environment variable not found.",
                                  "Is it O.K. to create", d, "? [y/N] " ) )
      if( substr( toupper( .create ), 1, 1 ) == "Y" ) {
        dir.create( d )
        if( !file.exists( d ) ) {
          stop( paste( "Sorry.  Could not create", d, "please check your system",
                       "permissions" ) ) ;
        }
      }
      else {
        stop( paste( "XMAP_BRIDGE_CACHE not set and no", d, "folder in home",
                     "directory. Please refer to the package installation",
                     "instructions for more details." ) )
      }
    }
  }
  d
}

.make.xmap.project.folder <- function() {
  .ok <- FALSE
  while( !.ok ) {
    .name <- file.path( .get.xmap.cache.dir(), paste( paste( format( Sys.time(), "%Y.%b.%d.%H.%M.%S" ), proc.time()[[1]], sep="_" ), ".xmb", sep="" ) )
    .ok <- !file.exists( .name )
  }
  dir.create( .name )
  .name
}

.validate.x.and.y <- function( x, y ) {
  if( missing( x ) ) {
    stop( "You need to specify values for x" )
  }
  if( !all( sapply( x, is.numeric ) ) ) {
    stop( "x seems to contain non-numerics" )
  }
  if( any( sapply( x, is.na ) ) ) {
    stop( "x cannot contain NA" )
  }
  if( any( sapply( x, is.nan ) ) ) {
    stop( "x cannot contain NaN" )
  }
  if( missing( y ) ) {
    stop( "You need to specify values for y" )
  }
  if( !all( sapply( y, is.numeric ) ) ) {
    stop( "y seems to contain non-numerics" )
  }
  if( any( sapply( y, is.na ) ) ) {
    stop( "y cannot contain NA" )
  }
  if( any( sapply( y, is.nan ) ) ) {
    stop( "y cannot contain NaN" )
  }
  if( length( x ) != length( y ) ) {
    stop( "'x' and 'y' lengths differ" )
  }
  if( length( x ) == 0 ) {
    stop( "lengths of 'x' and 'y' cannot be 0" )
  }
}

.make.xmap.child.file <- function( rootFolder, func ) {
  .x <- 1
  .ok <- FALSE
  .name <- ""
  .subname <- ""
  while( !.ok ) {
    .subname <- formatC( .x, width=8, flag="0" )
    .name <- file.path( rootFolder, .subname )
    .ok <- !file.exists( .name )
    .x <- .x + 1
  }
  func( .name )
  .subname
}

# Check to see if a folder is a valid project
.xmap.project.valid <- function( folder ) {
  file.exists( file.path( folder, "PROJECT" ) )
}

# Check to see if a folder is a valid graph
.xmap.graph.valid <- function( folder ) {
  file.exists( file.path( folder, "GRAPH" ) )
}

.touch.project <- function( itemid ) {
  .prj <- file.path( xmap.project.folder( itemid ), "PROJECT" )
  .data <- .xmap.read.file.info( .prj )
  .xmap.write.file.info( .prj, .data )
}

# read data from a file into a vector
.xmap.read.file.info <- function( file ) {
  if( is.character( file ) ) {
    file <- file( file, "r" )
    on.exit( close( file ) )
  }
  .ret <- list()
  if( !inherits( file, "connection" ) )
    stop( "'file' parameter must be a file object or a filename" )
  if( !isOpen( file, "r" ) ) {
    open( file, "r" )
    on.exit( close( file ) )
  }
  .ok <- TRUE
  while( .ok ) {
    .line <- readLines( file, 1 )
    if( length( .line ) == 0 ) {
      .ok <- FALSE
    }
    else if( .line[[1]] == '-' ) {
      .ok <- FALSE
    } 
    else {
      .elements <- strsplit( .line[[1]], "=" )
      .value <- paste( .elements[[1]][2:length(.elements[[1]])], collapse="=" )
      .ret[ .elements[[1]][1] ] <- tryCatch( { as.numeric( .value ) }, warning=function(e) { .value } )
    }
  }
  .ret
}

.xmap.write.file.info <- function( file, data ) {
  if( is.character( file ) ) {
    file <- file( file, "w" )
    on.exit( close( file ) )
  }
  .ret <- c()
  if( !inherits( file, "connection" ) )
    stop( "'file' parameter must be a file object or a filename" )
  if( !isOpen( file, "w" ) ) {
    open( file, "w" )
    on.exit( close( file ) )
  }
  data <- data[ sapply( data, function( a ) { !is.null( a ) } ) ]
  data <- lapply( data, function( a ) { ifelse( is.numeric( a ), format( a, scientific=FALSE ), a ) } )
  writeLines( paste( names( data ), data, sep="=" ), con=file )
}

###############################################################################
## Colour manipulation method (to add alpha)
###############################################################################

xmap.col <- function( col, alpha ) {
  c.ints <- col2rgb( col, alpha=T )

  if( !missing( alpha ) ) { 
    c.ints[4,] <- alpha
  }
  apply( c.ints, 2, function( a ) { ( a[4] * 256 * 256 * 256 ) +
                                    ( a[1] * 256 * 256 ) +
                                    ( a[2] * 256 ) +
                                      a[3] } )
}

###############################################################################
## Creation Methods
###############################################################################

xmap.plot.new <- function( graphid, name, x, y, type=c("scatter","line","bar","step","area","steparea"), col=NULL, dp=2 ) {
  if( !is( graphid, "Graph" ) ) {
    stop( paste( "graphid should be an object of class Graph, not", class( graphid ) ) )
  }
  type <- match.arg( type )
  if( is.character( col ) ) { col <- xmap.col(col) }
  .validate.x.and.y( x, y )
  .graph <- xmap.graph.folder( graphid )
  if( !.xmap.graph.valid( .graph ) ) {
    stop( paste( graphid, "is not a valid graph" ) )
  }
  .filename <- .make.xmap.child.file( .graph, file.create )
  .plot <- new( "Plot", project=graphid@project, graph=graphid@graph, minx=graphid@minx, plot=.filename )
  .data <- list( NAME=name, TYPE=type, COLOR=col )
  xmap.plot.save( .plot, .data, x, y, dp )
  .plot
}
xmap.graph.new <- function( projectid, name, desc, min, max, chr, start, stop, ylab="value", species=c("homo_sapiens", "mus_musculus", "rattus_norvegicus") ) {
  if( !is( projectid, "Project" ) ) {
    stop( paste( "projectid should be an object of class Project, not", class( projectid ) ) )
  }
  species <- match.arg( species )
  .prj <- xmap.project.folder( projectid )
  if( !.xmap.project.valid( .prj ) ) {
    stop( paste( projectid, "is not a valid project" ) )
  }

  if( is.nan( min ) )        { stop( "min cannot be NaN" ) ; }
  if( !is.numeric( min ) )   { stop( "min must be numeric" ) ; }
  if( is.nan( max ) )        { stop( "min cannot be NaN" ) ; }
  if( !is.numeric( max ) )   { stop( "min must be numeric" ) ; }
  if( is.nan( start ) )      { stop( "start cannot be NaN" ) ; }
  if( !is.numeric( start ) ) { stop( "start must be numeric" ) ; }
  if( is.nan( stop ) )       { stop( "stop cannot be NaN" ) ; }
  if( !is.numeric( stop ) )  { stop( "stop must be numeric" ) ; }
  if( start < 1 ) stop( "start cannot be less than 1" ) ;
  if( stop <= start ) stop( "stop must be greater than start" ) ;

  .data <- list( NAME=name, DESC=desc, MIN=min, MAX=max, CHR=chr, START=floor(start), STOP=ceiling(stop), YAXIS=ylab, SPECIES=species )
  .folder <- .make.xmap.child.file( .prj, dir.create )
  .path <- file.path( .prj, .folder, "GRAPH" )
  file.create( .path )
  .graph <- new( "Graph", project=projectid@project, graph=.folder, minx=start )
  xmap.graph.save( .graph, .data )
  .graph
}
xmap.project.new <- function( name ) {
  .folder <- .make.xmap.project.folder()
  .path <- file.path( .folder, "PROJECT" )
  file.create( .path )
  .data <- c( "NAME"=name )
  .project <- new( "Project", project=.folder )
  xmap.project.save( .project, .data )
  .project
}

###############################################################################
## Load Methods
###############################################################################

xmap.plot.load <- function( plotid ) {
  if( !is( plotid, "Plot" ) ) {
    stop( paste( "plotid should be an object of class Plot, not", class( plotid ) ) )
  }
  .xmap.read.file.info( xmap.plot.file( plotid ) )
}
xmap.graph.load <- function( graphid ) {
  if( !is( graphid, "Graph" ) ) {
    stop( paste( "graphid should be an object of class Graph, not", class( graphid ) ) )
  }
  .xmap.read.file.info( file.path( xmap.graph.folder( graphid ), "GRAPH" ) )
}
xmap.project.load <- function( projectid ) {
  if( !is( projectid, "Project" ) ) {
    stop( paste( "projectid should be an object of class Project, not", class( projectid ) ) )
  }
  .xmap.read.file.info( file.path( xmap.project.folder( projectid ), "PROJECT" ) )
}

###############################################################################
## Save Methods
###############################################################################

xmap.plot.save <- function( plotid, data, x, y, dp=2 ) {
  if( !is( plotid, "Plot" ) ) {
    stop( paste( "plotid should be an object of class Plot, not", class( plotid ) ) )
  }
  .validate.x.and.y( x, y )
  .file <- xmap.plot.file( plotid )
  .xmap.write.file.info( .file, data )
  write( "-", .file, append=TRUE )
  tryCatch( {
    mapply( function( a, b ) {
      a <- round( a - plotid@minx )
      b <- round( b, dp )
      r <- paste( paste( a, b, sep=",", collapse="," ), "\n", sep="", collapse="" )
      cat( r, file=.file, append=TRUE )
    }, x, y )
  }, warning=function(e) {
    stop( e$message )
  } )
  .touch.project( plotid )
}
xmap.graph.save <- function( graphid, data ) {
  if( !is( graphid, "Graph" ) ) {
    stop( paste( "graphid should be an object of class Graph, not", class( graphid ) ) )
  }
  .xmap.write.file.info( file.path( xmap.graph.folder( graphid ), "GRAPH" ), data )
  .touch.project( graphid )
}
xmap.project.save <- function( projectid, data ) {
  if( !is( projectid, "Project" ) ) {
    stop( paste( "projectid should be an object of class Project, not", class( projectid ) ) )
  }
  .xmap.write.file.info( file.path( xmap.project.folder( projectid ), "PROJECT" ), data )
}

###############################################################################
## List methods
###############################################################################

xmap.plot.list <- function( graphid ) {
  if( !is( graphid, "Graph" ) ) {
    stop( paste( "graphid should be an object of class Graph, not", class( graphid ) ) )
  }
  .files <- list.files( xmap.graph.folder( graphid ) )
  if( length( .files ) != 0 ) {
    .files <- .files[ sapply( .files, function( name ) { 
      name != "GRAPH"
    } ) ]
    sapply( .files, function( f ) {
      new( "Plot", project=graphid@project, graph=graphid@graph, minx=graphid@minx, plot=f )
    } )
  }
}
xmap.graph.list <- function( projectid ) {
  if( !is( projectid, "Project" ) ) {
    stop( paste( "projectid should be an object of class Project, not", class( projectid ) ) )
  }
  .prj <- xmap.project.folder( projectid )
  .files <- list.files( .prj )
  if( length( .files ) != 0 ) {
    .files <- .files[ sapply( .files, function( name ) { 
      .xmap.graph.valid( file.path( .prj, name ) ) 
    } ) ]
    .files <- sapply( .files, function( f ) { 
      f <- new( "Graph", project=projectid@project, graph=f, minx=0 )
    } )
    # load the minx value out of the data file
    sapply( .files, function( f ) {
      .info <- .xmap.read.file.info( file.path( xmap.graph.folder( f ), "GRAPH" ) )
      f@minx <- as.numeric( .info$START )
      f
    } )
  }
}
xmap.project.list <- function() {
  .files <- list.files( .get.xmap.cache.dir(), full.names=TRUE )
  if( length( .files ) != 0 ) {
    .files <- .files[ sapply( .files, .xmap.project.valid ) ]
    sapply( .files, function( f ) {
      f <- new( "Project", project=f )
    } )
  }
}

###############################################################################
## Delete Methods
###############################################################################

xmap.plot.delete <- function( plotid ) {
  if( !is( plotid, "Plot" ) ) {
    stop( paste( "plotid should be an object of class Plot, not", class( plotid ) ) )
  }
  unlink( xmap.plot.file( plotid ) )
}
xmap.graph.delete <- function( graphid ) {
  if( !is( graphid, "Graph" ) ) {
    stop( paste( "graphid should be an object of class Graph, not", class( graphid ) ) )
  }
  unlink( xmap.graph.folder( graphid ), recursive=TRUE )
}
xmap.project.delete <- function( projectid ) {
  if( !is( projectid, "Project" ) ) {
    stop( paste( "projectid should be an object of class Project, not", class( projectid ) ) )
  }
  unlink( xmap.project.folder( projectid ), recursive=TRUE )
}

###############################################################################
## Utility methods to let people use this like the existing plot and points
## graph methods
###############################################################################

.xmap.env <- new.env( hash=TRUE, parent=emptyenv() )

xmap.plot <- function( x, y, 
                      species=c("homo_sapiens", "mus_musculus", "rattus_norvegicus"), 
                      chr, xlim, ylim, main, sub, 
                      type=c("scatter","line","bar","step","area","steparea"), 
                      xlab, ylab="value", col, dp=2, projectid ) {
  .validate.x.and.y( x, y )
  species <- match.arg(species)
  if( missing( chr ) ) {
    stop( "You need to specify a chromosome for this plot" )
  }
  .project.created <- FALSE
  if( missing( projectid ) ) {
    projectid <- .xmap.env$project
    if( is.null( projectid ) || !.xmap.project.valid( xmap.project.folder( projectid ) ) ) {
      projectid <- xmap.project.new( format( Sys.time(), "Project created %d %b %Y" ) )
      .xmap.env$project <- projectid
      .project.created <- TRUE
    }
  }
  else {
    if( !is( projectid, "Project" ) ) {
      stop( paste( "projectid should be an object of class Project, not", class( projectid ) ) )
    }
  }
  tryCatch( {
    if( !.xmap.project.valid( xmap.project.folder( projectid ) ) ) {
      rm( "project", envir=.xmap.env )
      stop( paste( projectid, "is not a valid project" ) )
    }
    if( missing( col ) )  { col <- NULL }
    if( missing( sub ) )  { sub <- NULL }
    if( missing( main ) ) { main <- format( Sys.time(), "Graph created %H:%M:%S" ) }
    if( missing( xlim ) ) { xlim <- range( x ) }
    .xmap.env$xlim <- xlim
    if( missing( ylim ) ) { ylim <- range( c( 0, y ) ) }
    tryCatch( {
      graph <- xmap.graph.new( projectid, main, sub, ylim[ 1 ], ylim[ 2 ], chr, xlim[ 1 ], xlim[ 2 ], ylab, species )
      .xmap.env$graph <- graph
      .xmap.env$plotNo <- 1
      xmap.points( x, y, type, xlab, col, dp )
    },
    error=function( err ) {
      if( exists( "graph" ) ) {
        xmap.graph.delete( graph )
        rm( "graph", envir=.xmap.env )
      }
      stop( paste( err$message, "[dg]" ) )
    } )
  },
  error=function( err ) {
    if( .project.created ) {
      xmap.project.delete( projectid )
      rm( "project", envir=.xmap.env )
      stop( paste( err$message, "[dp]" ) )
    }
    stop( err$message )
  } )
}

xmap.points <- function( x, y, 
                        type=c("scatter","line","bar","step","area","steparea"), 
                        xlab, col, dp=2, graphid ) {
  .validate.x.and.y( x, y )
  if( missing( graphid ) ) {
    graphid <- .xmap.env$graph
    if( is.null( graphid ) || !.xmap.graph.valid( xmap.graph.folder( graphid ) ) ) {
      stop( "You must call xmap.plot or specify a graph to call xmap.points" )
    }
  }
  else {
    if( !is( graphid, "Graph" ) ) {
      stop( paste( "graphid should be an object of class Graph, not", class( graphid ) ) )
    }
  }
  if( !.xmap.project.valid( xmap.project.folder( graphid ) ) ) {
    stop( "Project for graph invalid" )
    rm( "project", envir=.xmap.env ) ;
  }
  .plotNo <- .xmap.env$plotNo
  if( is.null( .plotNo ) ) {
    .plotNo <- 1
  }
  if( missing( xlab ) ) { xlab <- paste( "Plot", .plotNo )  }
  if( missing( col ) ) { col <- NULL }
  tryCatch( {
    .plot <- xmap.plot.new( graphid, xlab, x, y, type, col, dp )
    .xmap.env$plotNo <- .plotNo + 1
    .plot
  },
  error=function( err ) {
    if( exists( ".plot" ) ) {
      xmap.plot.delete( .plot )
    }
    stop( err$message )
  } )
}

