#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

bfpackCorrelation <- function(jaspResults, dataset, options, ...) {


  # What type of BFpack analysis is being conducted?
  type <- "correlation"

  # apparently JASP can otherwise not find Fcor:
  data("Fcor", package = "BFpack")

  # Check if current options allow for analysis
  ready <- .bfpackOptionsReady(options, type)

  # handle the data set
  dataset <- .bfpackHandleMissings(dataset)

  # Check if current data allow for analysis
  .bfpackDataReady(dataset, options, type, ready)

  # Create a container for the results
  bfpackContainer <- .bfpackCreateContainer(jaspResults, deps = c("variables", "seed", "iterations",
                                                                  "manualHypotheses", "groupingVariable", "covariates"))

  .bfpackGetParameterEstimates(dataset, options, bfpackContainer, ready, type, jaspResults)

  # compute the results, aka BFs
  .bfpackComputeResults(dataset, options, bfpackContainer, ready, type)

  .bfpackParameterTable(options, bfpackContainer, type, dataset = dataset, position = 1)

  # Create a legend containing the order constrained hypotheses
  .bfpackLegendTable(options, type, bfpackContainer, position = 2)

  .bfpackMatrixTable(options, bfpackContainer, type, position = 3)

  .bfpackPosteriorHypothesesTable(options, bfpackContainer, type, position = 4)

  .bfpackSpecificationTable(options, bfpackContainer, type, position = 5)

  # coefficients table
  .bfpackEstimatesTable(options, bfpackContainer, type, position = 6)

  # standard hypotheses BF
  .bfpackStandardBfTable(options, bfpackContainer, type, position = 1.5)

  # Create the prior and posterior probability plots
  .bfpackPriorPosteriorPlot(options, bfpackContainer, type)

  # create the posterior distribution plot
  .bfpackPosteriorDistributionPlot(options, bfpackContainer, type)

  # create the traceplot
  .bfpackTraceplot(options, bfpackContainer, type)
}
