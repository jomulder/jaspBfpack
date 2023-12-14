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

Group
{
	property string parName: qsTr("mu")

	columns: 1
	// implicitHeight: 140 * preferencesModel.uiScale
	title: qsTr("Standard hypothesis test")

	Text { text: qsTr("Hypotheses   Prior probabilities")}
	ComponentsList 
	{
		implicitHeight: 90 * preferencesModel.uiScale
		implicitWidth: 200 * preferencesModel.uiScale
		source: [{
			values: [qsTr("H0: ") + parName + " = 0 ", qsTr("H1: ") + parName + " < 0 ", qsTr("H2: ") + parName + " > 0 "]
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