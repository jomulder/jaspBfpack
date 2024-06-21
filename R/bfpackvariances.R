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

bfpackVariances <- function(jaspResults, dataset, options, ...) {

  # sink(file = "~/Downloads/logBf.txt")
  # on.exit(sink(NULL))

  # What type of Bfpack analysis is being conducted?
  type <- "variances"

  # Check if current options allow for analysis
  ready <- .bfpackOptionsReady(options, type)

  # Read the data set
  dataList <- .bfpackReadDataset(options, type, dataset)

  # Check if current data allow for analysis
  .bfpackDataReady(dataList[["dataset"]], options, type)

  # Create a container for the results
  bfpackContainer <- .bfpackCreateContainer(jaspResults, deps = c("variables", "seed", "runAnalysisBox",
                                                                  "manualHypotheses"))

  .bfpackGetParameterEstimates(dataList, options, bfpackContainer, ready, type, jaspResults)

  # compute the results, aka BFs
  .bfpackComputeResults(dataList, options, bfpackContainer, ready, type)

  .bfpackParameterTable(options, bfpackContainer, type, position = 1)

  # Create a legend containing the order constrained hypotheses
  .bfpackLegendTable(options, type, bfpackContainer, position = 2)

  .bfpackMatrixTable(options, bfpackContainer, type, position = 3)

  .bfpackPosteriorHypothesesTable(options, bfpackContainer, type, position = 4)

  .bfpackSpecificationTable(options, bfpackContainer, type, position = 5)

  # coefficients table
  .bfpackCoefficientsTable(options, bfpackContainer, type, position = 6)

  # Create the prior and posterior probability plots
  .bfpackPriorPosteriorPlot(options, bfpackContainer, type)
}
