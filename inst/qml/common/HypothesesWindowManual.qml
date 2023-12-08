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
					// onCountChanged: 
					//         {
					//             for (var i = 0; i < count; i++)
					//             {
					//                 itemAt(i).visible = (i < 3)
					//                 if (i >= 3)
					//                     itemAt(i).width = 0
					//             }
					//         }
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