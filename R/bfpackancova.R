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

BfpackAncova <- function(jaspResults, dataset, options, ...) {

  # What type of Bfpack analysis is being conducted?
  type <- "ancova"

  # Check if current options allow for analysis
  ready <- .bfpackOptionsReady(options, type)

  # Read the data set
  dataList <- .bfpackReadDataset(options, type, dataset)

  # Check if current data allow for analysis
  .bfpackDataReady(dataList[["dataset"]], options, type)

  # Create a container for the results
  bfpackContainer <- .bfpackCreateContainer(jaspResults, deps = c("dependent", "fixedFactors", "covariates", "model", "seed"))

  # Create a legend containing the order constrained hypotheses
  .bfpackLegend(dataList[["dataset"]], options, type, jaspResults, position = 0)

  # Create a table containing the main analysis results
  .bfpackTestResultsTable(dataList[["dataset"]], options, bfpackContainer, dataList[["missing"]], ready, type, position = 1)

  # Create the Bayes factor matrix
  .bfpackBfMatrix(dataList[["dataset"]], options, bfpackContainer, ready, type, position = 2)

  # Create the descriptive statistics (coefficients) table
  .bfpackDescriptivesTable(dataList[["dataset"]], options, bfpackContainer, ready, type, position = 3)

  # Create the posterior probability plots
  .bfpackPosteriorProbabilityPlot(dataList[["dataset"]], options, bfpackContainer, ready, type, position = 4)

  # Create the descriptive plot(s)
  .bfpackDescriptivePlots(dataList[["dataset"]], options, bfpackContainer, ready, type, position = 5)
}
