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
			title: 						qsTr("Dependent Variables")
			allowedColumns: 			["scale"]
			heigth : 200*jaspTheme.uiScale
		}

		AssignedVariablesList
		{
			name: 						"fixedFactors"
			title: 						qsTr("Fixed Factors")
			allowedColumns: 			["ordinal", "nominal"]
		}

		AssignedVariablesList
		{
			name: 						"covariates"
			title: 						qsTr("Covariates")
			allowedColumns: 			["scale"]
		}
	}

	Common.HypothesesWindowStandard{
		parName: qsTr("mu")
	}
	
	Common.ParametersWindow{}


	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: true
		interactions: true
		anova: true
	}
}
