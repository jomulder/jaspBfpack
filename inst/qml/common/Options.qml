//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Section
{
	property bool bfTy: true
	property bool iterations: false
	property bool interactions: false
	property bool anova: false
	property bool samples: false
	property var interactionValues: []

	id: options
	title: 	qsTr("Options")

	Group
	{
		title: qsTr("Bayes Factor")
		// Layout.rowSpan: 2

		CheckBox
		{
			name: "logScale"
			label: qsTr("Log scale")
		}

		RadioButtonGroup
		{
			visible: bfTy
			name: "bfType"
			title: qsTr("Bayes factor type")
			radioButtonsOnSameRow: false
			RadioButton { value: "fractional"; 	label: qsTr("Fractional Bayes factor"); checked: true }
			RadioButton { value: "adjusted"; 		label: qsTr("Adjusted fractional Bayes factor"); 			}

		}
	}
	Group
	{
		title: 	qsTr("Tables")

		CheckBox 
		{
			name: "standardHypothesisBfTable"
			text: qsTr("Standard hypotheses BFs")
		}	

		CheckBox 
		{
			name: "specificationTable"
			text: qsTr("Specification")
		}

		CheckBox
		{
			name: 						"estimatesTable"
			text: 						qsTr("Estimates with uncertainty interval")
			childrenOnSameRow: true

			CIField
			{
				name: 					"ciLevel"
			}
		}
	}

	Group
	{
		
		title: qsTr("Plots")
		CheckBox
		{
			name: 						"manualPlots"
			text: 						qsTr("Manual hypotheses plots")
		}
		CheckBox
		{
			visible: 					samples
			name: 						"posteriorPlot"
			text: 						qsTr("Posterior plot")
		}
		CheckBox
		{
			visible: 					samples
			name: 						"traceplot"
			text: 						qsTr("Traceplot")
		}
	}

	Group
	{
		title: 							qsTr("Additional Options")

		IntegerField
		{
			visible: iterations
			name: "iterations"
			text: qsTr("No. iterations for parameter estimation")
			defaultValue: 5000
			min: 2000
			fieldWidth: 60 * preferencesModel.uiScale

		}

		DoubleField
		{
			name: 						"seed"
			text: 						qsTr("Seed")
			defaultValue: 				100
			min: 						-999999
			max: 						999999
			fieldWidth: 				60 * preferencesModel.uiScale
		}
	}


// there is still the issue how to get the covariates names and other variables names into this qml element
	function combinePairs(values) {
			var pairs = [];
			for (var i = 0; i < values.length; i++) {
					for (var j = i + 1; j < values.length; j++) {
							pairs.push(values[i] + ":" + values[j]);
					}
			}
			return pairs;
	}

	// Example usage
	property var interactionPairs: combinePairs(interactionValues);

	Group 
	{
		Layout.columnSpan: 2
		title: qsTr("Interaction terms")
		preferredWidth: 400 * jaspTheme.uiScale
		visible: interactions
		ComponentsList 
		{
			height: 120 * preferencesModel.uiScale
			headerLabels: [qsTr("Include")]
			name: "interactionTerms"
			values: interactionPairs
			addItemManually: false
			rowComponent: RowLayout { 
				Text { Layout.preferredWidth: 210*jaspTheme.uiScale; text: rowValue }
				CheckBox { Layout.preferredWidth: 50*jaspTheme.uiScale; name: "includeInteractionEffect"; checked: true }
			}
		}
	}

	Group 
	{
		title: qsTr("Effects")
		visible: anova
		columns: 2
		Text { text: "" }
		Text { text: qsTr("Prior weight") }
		Text { text: qsTr("Main zero effect") }
		FormulaField { name: "priorProbMainZero"; defaultValue: "1"; fieldWidth: 50 }
		Text { text: qsTr("Main non-zero effect") }
		FormulaField { name: "priorProbMainNonZero"; defaultValue: "1"; fieldWidth: 50 }
		Text { text: qsTr("Interaction zero effect") }
		FormulaField { name: "priorProbInteractionZero"; defaultValue: "1"; fieldWidth: 50 }
		Text { text: qsTr("Interaction non-zero effect") }
		FormulaField { name: "priorProbInteractionNonZero"; defaultValue: "1"; fieldWidth: 50 }
	}

}