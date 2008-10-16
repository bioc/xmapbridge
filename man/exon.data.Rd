\name{exon.data}
\alias{exon.data}

\title{ Sample exon array dataset }


\description{

  Data frame containing
  gene,exon and probeset IDs and probeset target locations for a subset of
  an Affmyetrix exon array dataset comparing two cell lines (mcf7 and
  mcf10a).
}

\usage{
data(xmapbridge)

}

\details{
  The object is a subset (80 genes, 1747 probesets) of RMA processed Exon
  Array data with associated annotation. It was generated using the
  exonmap bioconductor package. More details on the original dataset can
  be found in Okoniewski MJ, Miller CJ. Comprehensive Analysis of Affymetrix
  Exon Arrays
  Using BioConductor. PLoS Computational Biology 2008, 4(2): e6 doi:10.1371/journal.pcbi.0040006.
}

\value{
  On loading, creates the object exon.data.
}

\author{ Crispin J Miller }


\keyword{ misc }


