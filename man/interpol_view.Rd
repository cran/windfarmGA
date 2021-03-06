% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visibility.R
\name{interpol_view}
\alias{interpol_view}
\title{Plot an interpolated viewshed}
\usage{
interpol_view(
  res,
  plot = TRUE,
  breakseq,
  breakform = NULL,
  plotDEM = FALSE,
  fun = mean,
  pal = NULL,
  ...
)
}
\arguments{
\item{res}{The resulting list from \code{\link{viewshed}}}

\item{plot}{Should the result be plotted? Default is \code{TRUE}}

\item{breakseq}{The breaks for value plotting. By default, 5 equal intervals
are generated.}

\item{breakform}{If \code{breakseq} is missing, a sampling function to calculate
the breaks, like \code{\link{quantile}}, fivenum, etc.}

\item{plotDEM}{Plot the DEM? Default is \code{FALSE}}

\item{fun}{Function used for rasterize. Default is \code{mean}}

\item{pal}{A color palette}

\item{...}{Arguments passed on to \code{\link[raster]{plot}}.}
}
\value{
An interpolated raster
}
\description{
Plot an interpolated view of the viewshed analysis
}
\examples{
\dontrun{
library(raster)
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4496482, 4496482, 4499991, 4499991, 4496482),
    c(2666272, 2669343, 2669343, 2666272, 2666272)))),
  crs = 3035
))
DEM_meter <- getDEM(Polygon1)

turbloc = st_sample(DEM_meter[[2]], 10, type = "random");
res <- viewshed(r = DEM_meter[[1]], shape=DEM_meter[[2]], 
                turbine_locs = turbloc,  h1=1.8, h2=50)
interpol_view(res, plotDEM = TRUE)

interpol_view(res, breakseq = seq(0,max(colSums(res$Result)),1))
interpol_view(res, plotDEM = FALSE, breakform = quantile)
interpol_view(res, breakform = factor)

## Different color palettes
interpol_view(res, plotDEM = TRUE, pal=topo.colors)
interpol_view(res, plotDEM = TRUE, pal=colorRampPalette(c("white","purple")))

## ... Arguments are past on to the raster plot method
interpol_view(res, plotDEM = TRUE, alpha=0.5)
interpol_view(res, plotDEM = FALSE, breakseq = seq(0,10,1), colNA="black")

}
}
\seealso{
Other Viewshed Analysis: 
\code{\link{cansee}()},
\code{\link{plot_viewshed}()},
\code{\link{rasterprofile}()},
\code{\link{viewTo}()},
\code{\link{viewshed}()}

Other Plotting Functions: 
\code{\link{plot_cloud}()},
\code{\link{plot_development}()},
\code{\link{plot_evolution}()},
\code{\link{plot_fitness_evolution}()},
\code{\link{plot_heatmap}()},
\code{\link{plot_parkfitness}()},
\code{\link{plot_result}()},
\code{\link{plot_viewshed}()},
\code{\link{plot_windfarmGA}()},
\code{\link{plot_windrose}()},
\code{\link{random_search_single}()}
}
\concept{Plotting Functions}
\concept{Viewshed Analysis}
