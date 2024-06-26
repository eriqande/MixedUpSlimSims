% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mykiss_chroms}
\alias{mykiss_chroms}
\title{Chromosome raw and cumulative start and end points for rainbow trout (Oncorhynchus mykiss)}
\format{
A tibble with columns \code{chrom}, \code{start}, \code{end}, \code{cumul_start}, and \code{cumul_end}.
}
\source{
Omyk_v1.0 genome available on \href{https://www.ncbi.nlm.nih.gov/datasets/genome/GCF_002163495.1/}{NCBI}.
Eric got the fai via samtools and processed it into this form.
}
\usage{
mykiss_chroms
}
\description{
These are for the chromosomes from Omy_v1.0.  This is an example of the specification
that one should use for their chromosomes.
}
\keyword{datasets}
