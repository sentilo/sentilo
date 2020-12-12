var refreshIntervalMS = 1000 * 30;
var handlerId = undefined;
var selectedSensor = undefined;
var dataType = null;

var lastObsUrl = null;
var dataURLPrefix = null;
var ordersURLPrefix = null;
var alarmsURLPrefix = null;

var chartNav = null;

window.messages = {
	boolValues: {
		falseValue: 'no',
		trueValue: 'yes'
	},
	chart: {
		iniDate: 'Initial date:',
		endDate: 'End date:'
	}	
};

/**
 * Init all script variables needed by the graphs and data boxes
 */
var initComponentDetailVariables = function(url, dataUrl, ordersUrl, alarmsUrl, maxChartDate, type) {
	lastObsUrl = url;
	dataURLPrefix = dataUrl;
	ordersURLPrefix = ordersUrl;
	alarmsURLPrefix = alarmsUrl;
	theFuture = maxChartDate;
	to = theFuture;
	dataType = type;
}

/*
 * Updates alarm panel
 */

var makeAlarmLine = function(ph, alarm) {
	ph.append('<div class="activity_text_element"><span class="label label-important">' + alarm.timestamp + '</span>&nbsp;' + alarm.message + '</div>');
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
	ph.append('<div class="activity_text_element"><span class="label label-success">' + order.timestamp + '</span>&nbsp;' + order.order + '</div>');
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
	if (selectedSensor.dataType === 'LINK' || 
		selectedSensor.dataType === 'AUDIO_LINK' || 
		selectedSensor.dataType === 'IMAGE_LINK' || 
		selectedSensor.dataType === 'VIDEO_LINK' || 
		selectedSensor.dataType === 'FILE_LINK') {
		makeLinkChart(placeholder, url, selectedSensor.dataType, refreshChartControlLabels);
	} else if (selectedSensor.dataType === 'TEXT' || selectedSensor.dataType === 'JSON') {
		makeTextChart(placeholder, "#lastSensorDataPanel", url, selectedSensor.dataType, refreshChartControlLabels);
	} else if (selectedSensor.dataType === 'BOOLEAN') {
		chartNav = [];
		$(placeholder).empty();
		showBooleanDataChart(url, placeholder);
	} else {
		chartNav = [];
		$(placeholder).empty();
		showNumericDataChart(url, placeholder, selectedSensor.label, selectedSensor.unit);
	}
};

function showBooleanDataChart(url, dataPanel, filters) {
	jsonGET(url, filters, function(data) {
		var chartAnchor = $('<div id="bool-chart" class="ct-chart ct-perfect-fourth ct-chart-centered-labels"></div>');
		$(dataPanel).html(chartAnchor);
		printBooleanChart(data, '#bool-chart', {height: 220, bottomPadding: 20});
		
		var fromTimestamp = (data.fromTimestamp===null) ? '' : data.fromTimestamp;
		var toTimestamp = (data.toTimestamp===null) ? '' : data.toTimestamp;
		
		var iniDateMessage = window.messages.chart.iniDate;
		var endDateMessage = window.messages.chart.endDate;
		
		var chartControls = 
			'<div class="chart-controls row-fluid">' +
			'	<div class="chart-controls span3">' +
			'		<div class="btn-group">' +
		    '  			<button id="chart-control-prev-btn" class="btn"><i class="icon-chevron-left"></i></button>' +
		    '  			<button id="chart-control-refresh-btn" class="btn"><i class="icon-refresh"></i></button>' +
		    '  			<button id="chart-control-next-btn" class="btn"><i class="icon-chevron-right"></i></button>' +
		    '		</div>' +
			'	</div>' +
			'	<div class="chart-start-date span3"><span class="chart-controls-label">' + iniDateMessage + '&nbsp;</span><span class="chart-controls-value">' + fromTimestamp + '</span></div>' +
			'	<div class="chart-end-date span6"><span class="chart-controls-label">'+ endDateMessage +'&nbsp;</span><span class="chart-controls-value">' + toTimestamp + '</span></div>' +
			'</div>';
		
		$(dataPanel).append($(chartControls));
		
		$("#chart-control-prev-btn").unbind('click');
		$("#chart-control-prev-btn").on('click', function(e) {
			filters = {
				to: data.fromTime
			};
			var navActual = {
				fromTimestamp: data.fromTimestamp, 
				toTimestamp: data.toTimestamp, 
				fromTime: data.fromTime, 
				toTime: data.toTime
			};
			chartNav.push(navActual);
			showBooleanDataChart(url, dataPanel, filters);
		});
		
		$("#chart-control-refresh-btn").unbind('click');
		$("#chart-control-refresh-btn").on('click', function(e) {
			chartNav = [];
			filters = {};
			showBooleanDataChart(url, dataPanel, filters);
		});
		
		$("#chart-control-next-btn").unbind('click');
		$("#chart-control-next-btn").on('click', function(e) {
			var prevNav = chartNav.pop();
			if (!prevNav) {
				chartNav = [];
				filters = {};
			} else {
				filters = {
					to: prevNav.toTime + 1000
				};	
			}
			showBooleanDataChart(url, dataPanel, filters);
		});
		
	});
	
}

function showNumericDataChart(url, dataPanel, sensorType, unit, filters) {
	jsonGET(url, filters, function(data) {
		var chartAnchor = $('<div id="num-chart" class="ct-chart ct-perfect-fourth ct-chart-centered-labels"></div>');
		$(dataPanel).html(chartAnchor);
		printNumericalChart(data, sensorType, unit, '#num-chart', {height: 220, bottomPadding: 20});
		
		var fromTimestamp = (data.fromTimestamp===null) ? '' : data.fromTimestamp;
		var toTimestamp = (data.toTimestamp===null) ? '' : data.toTimestamp;
		
		var iniDateMessage = window.messages.chart.iniDate;
		var endDateMessage = window.messages.chart.endDate;
		
		var chartControls = 
			'<div class="chart-controls row-fluid">' +
			'	<div class="chart-controls span3">' +
			'		<div class="btn-group">' +
		    '  			<button id="chart-control-prev-btn" class="btn"><i class="icon-chevron-left"></i></button>' +
		    '  			<button id="chart-control-refresh-btn" class="btn"><i class="icon-refresh"></i></button>' +
		    '  			<button id="chart-control-next-btn" class="btn"><i class="icon-chevron-right"></i></button>' +
		    '		</div>' +
			'	</div>' +
			'	<div class="chart-start-date span3"><span class="chart-controls-label">'+ iniDateMessage +'&nbsp;</span><span class="chart-controls-value">' + fromTimestamp + '</span></div>' +
			'	<div class="chart-end-date span6"><span class="chart-controls-label">'+ endDateMessage +'&nbsp;</span><span class="chart-controls-value">' + toTimestamp + '</span></div>' +
			'</div>';
		
		$(dataPanel).append($(chartControls));
		
		$("#chart-control-prev-btn").unbind('click');
		$("#chart-control-prev-btn").on('click', function(e) {
			filters = {
				to: data.fromTime
			};
			var navActual = {
				fromTimestamp: data.fromTimestamp, 
				toTimestamp: data.toTimestamp, 
				fromTime: data.fromTime, 
				toTime: data.toTime
			};
			chartNav.push(navActual);
			showNumericDataChart(url, dataPanel, sensorType, unit, filters);
		});
		
		$("#chart-control-refresh-btn").unbind('click');
		$("#chart-control-refresh-btn").on('click', function(e) {
			chartNav = [];
			filters = {};
			showNumericDataChart(url, dataPanel, sensorType, unit, filters);
		});
		
		$("#chart-control-next-btn").unbind('click');
		$("#chart-control-next-btn").on('click', function(e) {
			var prevNav = chartNav.pop();
			if (!prevNav) {
				chartNav = [];
				filters = {};
			} else {
				filters = {
					to: prevNav.toTime + 1000
				};	
			}
			showNumericDataChart(url, dataPanel, sensorType, unit, filters);
		});
		
	});
	
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

/*
 * Refresh last sensor observation panel.
 */

var addLastSensorObservationToPanel = function(panel, data) {
	if (data.dataType === 'AUDIO_LINK') {
		showAndLoadAudioPlayer(panel, data.value, data.formattedValue, data.sensor, data.sensorType, data.timestamp);
		stopPreviousRefresh();
	} else if (data.dataType === 'VIDEO_LINK') {
		showAndLoadVideoPlayer(panel, data.value, data.formattedValue, data.sensor, data.sensorType, data.timestamp);
		stopPreviousRefresh();
	} else if (data.dataType === 'IMAGE_LINK') {
		showImage(panel, data.value, data.formattedValue, data.sensor, data.sensorType, data.timestamp);
		stopPreviousRefresh();
	} else if (data.dataType === 'FILE_LINK') {
		var filename = data.value.substring(data.value.lastIndexOf('/')+1);
		showFile(panel, data.value, data.formattedValue, filename, data.sensor, data.sensorType, data.timestamp);
		stopPreviousRefresh();
	} else if (data.dataType === 'LINK') {
		showLink(panel, data.value, data.sensor, data.sensorType, data.timestamp);
	} else if (data.dataType === 'JSON') {
		showJson(panel, data.value, data.timestamp);
	} else {
		var stats = $('<div id="componentLastObsValue" class="stats"></div>');
		var date = '<br/> <spring:message code="component.sensor.observation.lastupdated"/> <br/>';
		if (data.found) {
			date = date + data.timestamp;
			if (data.dataType == 'BOOLEAN') {
				data.value = eval(data.value) ? messages.boolValues.trueValue.toUpperCase() : messages.boolValues.falseValue.toUpperCase();
				stats.html(data.value + ' ' + data.unit);
			} else {
				stats.html(data.value + ' ' + data.unit);
			} 
		} else {
			date = date + '<spring:message code="component.sensor.observation.not.found"/>';
			stats.html('<spring:message code="component.sensor.observation.not.found"/>');
		}
		panel.append(data.sensor + '<br/> ' +data.sensorType + '<br/>');
		panel.append(stats);
		panel.append(date);
		panel.append($('<br/>'));
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
		
	// Retrieve chart data
	if (dataType === 'data') {
		retrieveActivity('#activity_placeholder', createChartUrl(dataURLPrefix + selectedSensor.id + "/", from, to), selectedSensor.label);
	} else if (dataType === 'orders') {
		retrieveOrders('#orders_placeholder', createChartUrl(ordersURLPrefix + selectedSensor.id + "/", from, to));
	} else if (dataType === 'alarms') {
		retrieveAlarms('#alarms_placeholder', createChartUrl(alarmsURLPrefix + selectedSensor.id + "/", from, to));
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
	changeDataType('data');
}