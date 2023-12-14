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

Form
{
	VariablesForm
	{
		AvailableVariablesList
		{
			name: 						"variablesList"
		}

		AssignedVariablesList
		{
			name: 						"dependent"
			title: 						qsTr("Dependent Variable")
			allowedColumns: 			["scale"]
		}

		AssignedVariablesList
		{
			name: 						"fixedFactors"
			title: 						qsTr("Fixed Factors")
			allowedColumns: 			["ordinal", "nominal", "nominalText"]
		}

		AssignedVariablesList
		{
			name: 						"covariates"
			title: 						qsTr("Covariates")
			allowedColumns: 			["ordinal", "nominal", "scale"]
		}
	}

	CheckBox
	{
		Layout.columnSpan: 2
		id: 						runAnalysisBox
		name: 					"runAnalysisBox"
		label: 					qsTr("<b>Run Analysis</b>")
		checked: 				false
		Component.onCompleted:
		{
			background.color = "#ff8600"
		}
	}

	Group
	{
		columns: 1
		// implicitHeight: 140 * preferencesModel.uiScale
		title: qsTr("Standard hypothesis test")

		Text { text: qsTr("Hypotheses   Prior probabilities")}
		ComponentsList 
		{
			implicitHeight: 90 * preferencesModel.uiScale
			implicitWidth: 200 * preferencesModel.uiScale
			source: [{
				values: [qsTr("H0: mu = 0 "), qsTr("H1: mu < 0 "), qsTr("H2: mu > 0 ")]
			}]
			name: "standardHypotheses"
			// titles: [qsTr("Hypotheses"), qsTr("Prior probabilities")]
			rowComponent: RowLayout {
				Text { text: rowValue }
				FormulaField {
					implicitWidth: 100 * preferencesModel.uiScale
					name: "priorProb"
					fieldWidth: 50
					defaultValue: "1/3"
					}
			}
		}
	}
	Group
	{
		columns: 1
		title: qsTr("Manual hypothesis test")

		ComponentsList {
			name: "hiddenNames"
			id: hiddenNames
			// source: [{values: ["est1", "est2", "est3"]}]
			rSource: "estimateNamesForQml"
			visible: false
		}
		Text { text: qsTr("Once you drag variables to the analysis window the names \nof the estimates that you may use to specify manual \nhypotheses will display here:") }
		Flow
		{
			width: parent.width
			// anchors.margins: 1
			spacing: 5
			Repeater
			{
						model: hiddenNames.model // estimates must be the id of the hidden ComponentList
						TextEdit {
							text: model.name
							readOnly: true
							wrapMode: Text.WordWrap
							selectByMouse: true
						}
			}
		}

		Text { text: qsTr("<b>An example may be</b>: ")}

		InputListView
		{
			name				: "manualHypotheses"
			title				: qsTr("Hypotheses")
			optionKey			: "name"
			defaultValues		: ["...", "..."]
			placeHolder			: qsTr("New hypothesis")
			minRows				: 2

			// preferredWidth		: (2*form.width)/3
			preferredHeight: 100 * preferencesModel.uiScale
			rowComponentTitle	: qsTr("Prior probabilities")
			rowComponent: FormulaField
			{
				fieldWidth: 60
				name: "priorProbManual"
				defaultValue: "1/2"
			}
		}

		CheckBox {
			name: "complement"
			label: qsTr("Complement:")
			checked: true
			childrenOnSameRow: true
			FormulaField {
				fieldWidth: 60
				name: "priorProbComplement"
				label: qsTr("Prior Probability")
				defaultValue: "1/2"
			}
		}
	}	
	// Common.HypothesesWindowManual{}

	Section
	{
		title: 	qsTr("Options")
		Group
		{
			title: qsTr("Bayes Factor")
			Layout.rowSpan: 2

			CheckBox
			{
				name: "logScale"
				label: qsTr("On log scale")
			}

			RadioButtonGroup
			{
				// set visible to false to still get the option set in R
				visible: true
				name: "bfType"
				title: qsTr("Type")
				radioButtonsOnSameRow: false
				RadioButton { value: "fractional"; label: qsTr("Fractional"); checked: true}
				RadioButton { value: "adjusted"; label: qsTr("Adjusted fractional")}
			}
		}
		Group
		{
			title: 							qsTr("Tables")

			CheckBox 
			{
				name: "specificationTable"
				text: qsTr("Specification")
			}

			CheckBox
			{
				name: 						"coefficientsTable"
				text: 						qsTr("Coefficients")

				CIField
				{
					name: 					"ciLevel"
					text: 					qsTr("Credible interval")
				}
			}
		}

		Group
		{
			

			CheckBox
			{
				name: 						"plots"
				text: 						qsTr("Manual hypotheses plots")
			}
		}

		Group
		{
			title: 							qsTr("Additional Options")

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
	}
}
