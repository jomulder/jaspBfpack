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
import JASP.Widgets
import QtQuick.Controls as QTCONTROLS

Group
{

	Layout.columnSpan: 2

	Layout.fillWidth: true

	preferredWidth: 590 * jaspTheme.uiScale 

	columns: 1
	RowLayout {
		Label { text: qsTr("<b>Manual hypothesis test</b>") }
		HelpButton
		{
			toolTip: 					qsTr("Click for more information")
			helpPage:					"forQml/tooltip"
		}
	}
	ComponentsList
	{
		
		name: "manualHypotheses"
		title: ""
		minimumItems: 1
		headerLabels: [qsTr("Hypotheses"), qsTr("Prior weight"), qsTr("Include")]
		rowComponent: 
			RowLayout {
				TextField
				{ 
					name: "hypothesisText"
					placeholderText: "..."
					fieldWidth: 400 * jaspTheme.uiScale
				}
				FormulaField
				{
					fieldWidth: 60
					name: "priorProbManual"
					defaultValue: "1"
				}
				Item {}
				CheckBox
				{
					name: "includeHypothesis"
				}
			}
		addBorder: false
	}

	Item
	{
		width: 536 * jaspTheme.uiScale
		height: complement.height
		Label { text: qsTr("Complement hypothesis:") }
		FormulaField {
			anchors.right: spacer.left
			fieldWidth: 60
			name: "priorProbComplement"
			defaultValue: "1"
		}
		Item {
			id: spacer
			width: 30 // Adjust the width as needed for the desired space
			anchors.right: complement.left
		}
		CheckBox {
			anchors.right: parent.right
			id: complement
			name: "complement"
			checked: true
		}
	}
}
