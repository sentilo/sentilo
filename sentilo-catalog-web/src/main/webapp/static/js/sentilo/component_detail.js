var refreshIntervalMS = 1000 * 30;
var handlerId = undefined;
var selectedSensor = undefined;
var dataType = null;

var lastObsUrl = null;
var activityURLPrefix = null;
var ordersURLPrefix = null;
var alarmsURLPrefix = null;

/**
 * Init all script variables needed by the graphs and data boxes
 */
var initComponentDetailVariables = function(url, activityUrl, ordersUrl, alarmsUrl, maxChartDate, type, numObs) {
	lastObsUrl = url;
	activityURLPrefix = activityUrl;
	ordersURLPrefix = ordersUrl;
	alarmsURLPrefix = alarmsUrl;
	theFuture = maxChartDate;
	to = theFuture;
	dataType = type;
	chartVisibleObservationsNumber = numObs;
}

/*
 * Updates alarm panel
 */

var makeAlarmLine = function(ph, alarm) {
	ph.append('<div class="activity_text_element"><span class="label label-important">' + formatTimestamp(alarm.timestamp) + '</span>&nbsp;' + alarm.message + '</div>');
};
 
var retrieveAlarms = function(placeholder, url) {
	var ph = $(placeholder);
	ph.empty();
	jsonGET(url, [], function(data) {
		for(var i = 0; i < data.length; i++) {
			var alarm = data[i];
			makeAlarmLine(ph, alarm);
		}
	});
};

/*
 * Updates orders panel
 */
var makeOrderLine = function(ph, order) {
	ph.append('<div class="activity_text_element"><span class="label label-success">' + formatTimestamp(order.timestamp) + '</span>&nbsp;' + order.order + '</div>');
};
 
var retrieveOrders = function(placeholder, url) {
	var ph = $(placeholder);
	ph.empty();
	jsonGET(url, [], function(data) {
		for(var i = 0; i < data.length; i++) {
			var order = data[i];
			makeOrderLine(ph, order);
		}
	});
};

/*
 * Updates activity panel
 */
var retrieveActivity = function(placeholder, url, label) {
	if (selectedSensor.dataType == 'BOOLEAN') {
		makeBooleanChart(placeholder, url, label, refreshChartControlLabels);
	} else if (selectedSensor.dataType == 'TEXT') {
		makeTextChart(placeholder, url, label, refreshChartControlLabels);
	} else {
		makeNumberChart(placeholder, url, label, refreshChartControlLabels);
	}
};

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

/*
 * Refresh last sensor observation panel.
 */

var addLastSensorObservationToPanel = function(panel, data) {

	var isJsonValue = false;
	var stats = $('<div id="componentLastObsValue" class="stats"></div>');
	var date = '<br/> <spring:message code="component.sensor.observation.lastupdated"/> <br/>';
	if (data.found) {
		date = date + formatTimestamp(data.timestamp);
					
		if (data.dataType == 'BOOLEAN') {
			data.value = eval(data.value) ? '' : 'No';
			stats.html(data.value + ' ' + data.unit);
		} else if (data.dataType == 'TEXT') {
			if (!$.isNumeric(data.value) && validateJson(data.value)) {
				// stats.html('<pre class="json">' + jsonPrettyPrint.toHtml(jQuery.parseJSON(data.value)) + '</pre>');
				isJsonValue = true;
			} else {
				stats.html(data.value + ' ' + data.unit);
			}
		} else {
			stats.html(data.value + ' ' + data.unit);
		}
		
	} else {
		date = date + '<spring:message code="component.sensor.observation.not.found"/>';
		stats.html('<spring:message code="component.sensor.observation.not.found"/>');
	}
	panel.append(data.sensor+'<br/>'+data.sensorType+'<br/>');
	panel.append(stats);
	panel.append(date);
	panel.append($('<br/>'));
	
	if (isJsonValue) {
		$("#componentLastObsValue").removeClass("stats");
		
		var jsonContainer = $('#componentLastObsValue');
		jsonContainer
			.jsonPresenter('destroy')
			.jsonPresenter({ json: jQuery.parseJSON(data.value) })
			.jsonPresenter('expand', 0);
		
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
				
		
		// Append buttons
		actionButtonsToolbar.append(actionButtonsGrp);
		jsonContainer.append(actionButtonsToolbar);
		jsonContainer.jsonPresenter('expandAll');
	}
	
};

var retrieveLastSensorObservationPanel = function() {

	var panel = $('#lastSensorDataPanel');
	panel.empty();

	var url = lastObsUrl + selectedSensor.id + "/";
	jsonGET(url, [], function(data) {
		addLastSensorObservationToPanel(panel, data);
	});
};

/*
 * Refresh activity, orders and alarms
 */

var retrieveChartPanel = function() {
	
	// Get sensor's visual configuration
	// Sensor > Tenant > Default
	var limit = selectedSensor.visualConfiguration.chartVisibleObservationsNumber;
	if (typeof limit === 'undefined' || limit === null || limit === '') { 
		if (typeof chartVisibleObservationsNumber !== 'undefined' && chartVisibleObservationsNumber !== null && chartVisibleObservationsNumber !== '') {
			limit = chartVisibleObservationsNumber;
		} 
	}
	
	// Retrieve chart data
	if (dataType === 'activity') {
		retrieveActivity('#activity_placeholder', createChartUrl(activityURLPrefix + selectedSensor.id + "/", from, to, limit), selectedSensor.label);
	} else if (dataType === 'orders') {
		retrieveOrders('#orders_placeholder', createChartUrl(ordersURLPrefix + selectedSensor.id + "/", from, to, limit));
	} else if (dataType === 'alarms') {
		retrieveAlarms('#alarms_placeholder', createChartUrl(alarmsURLPrefix + selectedSensor.id + "/", from, to, limit));
	}
	
}

/*
 * Refresh data delegate 
 */
var refreshData = function() {
	retrieveLastSensorObservationPanel();
	retrieveChartPanel();
};

/*
 * stopPreviousRefresh: Stops the timer thread if started.
 */
var stopPreviousRefresh = function() {
	if (handlerId) {
		clearInterval(handlerId);
	}
}

/*
 * restartRefreshingData: Starts the timer thread.
 */
var restartRefreshingData = function() {
	stopPreviousRefresh();
	refreshData();
	handlerId = setInterval(refreshData, refreshIntervalMS);
};

/*
 * Changes selected data type
 */
var changeDataType = function(type) {
	dataType = type;
	restartRefreshingData();
}

/*
 * Changes current selected sensor
 */
var selectSensor = function(sensor) {
	selectedSensor = sensor;
	changeDataType('activity');
}