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
import QtQuick.Controls  as QTCONTROLS



Group
{
	columns: 1
	title: qsTr("<b>Manual hypothesis test</b>")

	ComponentsList {
		name: "hiddenNames"
		id: hiddenNames
		// source: [{values: ["est1", "est2", "est3"]}]
		rSource: "estimateNamesForQml"
		visible: false
	}

	RowLayout {
		Text {
			text: qsTr("<b>Parameters:</b>")
			MouseArea
			{
					id: mouseAreaA
					anchors.fill: parent
					cursorShape: Qt.IBeamCursor
					hoverEnabled: true
			}
			QTCONTROLS.ToolTip.visible: mouseAreaA.containsMouse
			//QTCONTROLS.ToolTip.delay: 300
			QTCONTROLS.ToolTip.text: qsTr("The parameters that can be tested will appear here once you have entered variables into the analysis")	
		}
		HelpButton
		{
			toolTip: 					qsTr("Click for more information")
			helpPage:					"forQml/tooltip"
		}
	}

	Flow
	{
		// width: parent.width
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


	InputListView
	{
		name				: "manualHypotheses"
		title				: qsTr("Hypotheses")
		optionKey			: "name"
		defaultValues		: ["...", "..."]
		placeHolder			: qsTr("New hypothesis")
		minRows				: 1

		// preferredWidth: 100 * preferencesModel.uiScale
		preferredHeight: 100 * preferencesModel.uiScale
		rowComponentTitle	: qsTr("Prior weights")
		rowComponent: FormulaField
		{
			fieldWidth: 40
			name: "priorProbManual"
			defaultValue: "1"
		}
	}

	CheckBox {
		name: "complement"
		label: qsTr("Complement:		                       ")
		checked: true
		childrenOnSameRow: true
		FormulaField {
			fieldWidth: 40
			name: "priorProbComplement"
			// label: qsTr("Prior weight")
			defaultValue: "1"
		}
	}
}