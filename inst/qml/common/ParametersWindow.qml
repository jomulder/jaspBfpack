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

	ComponentsList {
		name: "hiddenNames"
		id: hiddenNames
		rSource: "estimateNamesForQml"
		visible: false
	}

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
		QTCONTROLS.ToolTip.text: qsTr("The parameters that can be tested in the manual hypotheses will appear here once you have entered variables into the analysis")	
	}
	

	ListView
	{
		// width: parent.width
		// anchors.margins: 1
		spacing: 5
		height: 80 * jaspTheme.uiScale 
		width: 300 * jaspTheme.uiScale

		QTCONTROLS.ScrollBar.vertical: QTCONTROLS.ScrollBar { policy: QTCONTROLS.ScrollBar.AlwaysOn}
		QTCONTROLS.ScrollBar.horizontal: QTCONTROLS.ScrollBar {}
		model: hiddenNames.model
		delegate: TextEdit {
		 		text: model.name
		 		readOnly: true
		 		wrapMode: Text.WordWrap
		 		selectByMouse: true
    }
	}
	
}
