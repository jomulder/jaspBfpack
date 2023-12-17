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
		implicitHeight: 200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name: 						"variablesList"
		}
		
		AssignedVariablesList
		{	
			title: 						qsTr("Variable")
			name: 								"variables"
			singleVariable: 			true
			allowedColumns: 			["scale", "ordinal"]
		}
		AssignedVariablesList
		{
			name: 						"groupingVariable"
			title: 						qsTr("Grouping Variable")
			singleVariable: 			true
			allowedColumns: 			["nominal", "nominalText"]
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
		parName: qsTr("delta")
		bartlett: true
	}

	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: false
	}
}
