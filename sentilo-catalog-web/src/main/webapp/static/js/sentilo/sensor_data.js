var dataType = null;
var sensorUnit = null;
var limit = null;
var lastObsUrl = null;

var initSensorDataVariables = function(type, unit, obsLimit, url, future) {
	dataType = type;
	sensorUnit = unit;
	limit = obsLimit;
	lastObsUrl = url;
	theFuture = future;
	from = null;
	to = theFuture;
	oldToList = [];
}

var retrieveChartPanel = function() {
	
	var url = createChartUrl(lastObsUrl, from, to, limit);
	
	if (dataType === 'NUMBER') {
		makeNumberChart('#plot', url, sensorUnit, refreshChartControlLabels);
	} else if (dataType === 'BOOLEAN') {
		makeBooleanChart('#plot', url, sensorUnit, refreshChartControlLabels);
	} else if (dataType === 'TEXT') {
		makeTextChart('#plot', url, sensorUnit, refreshChartControlLabels);
	} 
	
}

var refreshChartControlLabels = function(theNavData) {
	
	// This is a chart callback function
	// When data is loaded onto chart, calls this function
	// for update window labels
	// There's an important issue with delays after chart is updated :-(
	
	if (theNavData) {
		// Update the control labels
		$("#chartToDate").text((theNavData.startDate !== 'undefined' && theNavData.startDate !== null) ? theNavData.startDate : '');
		$("#chartFromDate").text((theNavData.endDate !== 'undefined' && theNavData.endDate !== null) ? theNavData.endDate : '');
		$("#numObservations").text(theNavData.numItems);
	}
}

function printLastSensorObservationValue(dataType, value) {
	
	// var dataType = '${sensor.dataType}';
	// var value = '${sensorLastObservation.value}';
	var isJsonValue = false;
	
	if (dataType === 'TEXT') {
		if (!$.isNumeric(value) && validateJson(value)) {
			isJsonValue = true;
		} else {
			isJsonValue = false;
		}
	}
	
	if (isJsonValue) {
		
		var jsonContainer = $('#sensorLastObsValue');
		jsonContainer
			.jsonPresenter('destroy')
			.jsonPresenter({ json: jQuery.parseJSON(value) })
			.jsonPresenter('expandAll');
		
		var actionButtonsToolbar = $('<div class="btn-toolbar pull-right"><i class="icon-align-left"></i></a>');
		var actionButtonsGrp = $('<div class="btn-group" />');
				
		// Collapse all button
		actionButtonsGrp.append($('<a class="btn" href="#" id="collapse-all"><i class="icon-resize-small"></i></a>')
			.on('click', function() {
				jsonContainer.jsonPresenter('collapseAll');
			})
		);
		
		// Expand all button
		actionButtonsGrp.append($('<a class="btn" href="#" id="expand-all"><i class="icon-resize-full"></i></a>')
			.on('click', function() {
				jsonContainer.jsonPresenter('expandAll');
			})
		);
		
		// Expand n levels button
		actionButtonsGrp.append($('<a class="btn" href="#" id="expand-levels"><i class="icon-align-left"></i></a>')
			.on('click', function() {
				var levels = parseInt($('#levels').val());
				jsonContainer.jsonPresenter('expand', levels);
			})
		);
		
		// Expandable levels
		actionButtonsToolbar.append($('<input type="text" id="levels" value="0" class="pull-right" />'));
		
		// Append buttons
		actionButtonsToolbar.append(actionButtonsGrp);
		jsonContainer.append(actionButtonsToolbar);
	} else {
		$("#sensorLastObsValue").html(value);	
	}
	
}