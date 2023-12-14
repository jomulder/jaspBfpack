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
import "./common" as Common

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
			singleVariable: 			true
			allowedColumns: 			["scale"]
		}

		AssignedVariablesList
		{
			name: 						"covariates"
			title: 						qsTr("Covariates")
			suggestedColumns: 			["scale"]
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

	Common.HypothesesWindowStandard{
		parName: qsTr("beta")
	}

	Common.HypothesesWindowManual{}

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
					text: 					qsTr("Uncertainty interval")
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
