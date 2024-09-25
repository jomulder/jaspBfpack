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
	property bool multigroup: false

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
			RadioButton { value: "adjusted"; label: qsTr("Adjusted fractional Bayes factor"); checked: true}
			RadioButton { value: "fractional"; label: qsTr("Fractional Bayes factor")}

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
			name: 						"plots"
			text: 						qsTr("Manual hypotheses plots")
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

	Group 
	{
		Layout.columnSpan: 2
		title: qsTr("Interaction terms")
		preferredWidth: 300 * jaspTheme.uiScale
		visible: interactions
		ComponentsList 
		{
			height: 120 * preferencesModel.uiScale
			headerLabels: [qsTr("Include")]
			// titles: [qsTr("Include")]
			name: "interactionTerms"
			rSource: "interactionSource"
			// source: "covariates"
			rowComponent: RowLayout { 
				Text {Layout.preferredWidth: 210*jaspTheme.uiScale; text: rowValue; visible: true}
				CheckBox {Layout.preferredWidth: 50*jaspTheme.uiScale; name: "includeInteractionEffect"; checked:true}
			}
		}
	}

	Group {
		visible: multigroup
		title: qsTr("Multigroup")
		DropDown
		{
			label: qsTr("Grouping variable");
			name: "group";
			showVariableTypeIcon: true;
			addEmptyValue: true;
		} // No model: it takes all variables per default
	}

}