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
		preferredHeight: 200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name: 						"variablesList"
		}
		
		AssignedVariablesList
		{
			name: 								"variables"
			singleVariable: 			false
			allowedColumns: 			["scale", "ordinal"]
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
		parName: qsTr("rho")
	}

	Common.ParametersWindow{}
	
	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: false
		iterations: true
		multigroup: true
	}

	Section
	{
		title: qsTr("Covariates")
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale

			AvailableVariablesList
			{
				name: 						"covariatesList"
				source: 					"variablesList"
			}
			
			AssignedVariablesList
			{
				name: 								"covariates"
				singleVariable: 			false
				allowedColumns: 			["scale", "ordinal"]
			}
		}
	
	}
}
