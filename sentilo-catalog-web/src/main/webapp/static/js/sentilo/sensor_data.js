var sensor = null;
var sensorType = null;
var dataType = null;
var sensorUnit = null;
var lastObsUrl = null;
var chartNav = [];

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

var initSensorDataVariables = function(type, dType, unit, url) {
	sensorType = type;
	dataType = dType;
	sensorUnit = unit;	
	lastObsUrl = url;
}

var retrieveChartPanel = function() {
	
	var url = createChartUrl(lastObsUrl);
	var placeholder = "#plot";
	var valuePlaceholder = '#sensorLastObsValue';
	var label = '';
	
	if (dataType === 'LINK' || 
		dataType === 'AUDIO_LINK' || 
		dataType === 'IMAGE_LINK' || 
		dataType === 'VIDEO_LINK' || 
		dataType === 'FILE_LINK') {
		makeLinkChart(placeholder, url, dataType, refreshChartControlLabels);
	} else if (dataType === 'TEXT' || dataType === 'JSON') {
		makeTextChart(placeholder, valuePlaceholder, url, dataType, refreshChartControlLabels);
	} else if (dataType === 'NUMBER') {
		showNumericDataChart(url, placeholder, sensorType, sensorUnit);
	} else if (dataType === 'BOOLEAN') {
		showBooleanDataChart(url, placeholder)
	} 
	
}

function showNumericDataChart(url, dataPanel, sensorType, unit, filters) {
	jsonGET(url, filters, function(data) {
		var chartAnchor = $('<div id="num-chart" class="ct-chart ct-perfect-fourth ct-chart-centered-labels"></div>');
		$(dataPanel).html(chartAnchor);
		printNumericalChart(data, sensorType, unit, '#num-chart', {height: 240, bottomPadding: 40});
		
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

function printLastSensorObservationValue(sensorId, dataType, value, formattedValue, timestamp) {
	
	var panel = $('#sensorLastObsValue');
	
	if (dataType === 'AUDIO_LINK') {
		showAndLoadAudioPlayer(panel, value, formattedValue, sensor, sensorType, timestamp);
	} else if (dataType === 'VIDEO_LINK') {
		showAndLoadVideoPlayer(panel, value, formattedValue, sensor, sensorType, timestamp);
	} else if (dataType === 'IMAGE_LINK') {
		showImage(panel, value, formattedValue, sensor, sensorType, timestamp);
	} else if (dataType === 'FILE_LINK') {
		var filename = value.substring(value.lastIndexOf('/')+1);
		showFile(panel, value, formattedValue, filename, sensor, sensorType, timestamp);
	} else if (dataType === 'LINK') {
		showLink(panel, value, sensor, sensorType, timestamp);
	} else if (dataType === 'JSON') {
		showJson(panel, value, timestamp);
	} else if (dataType === 'BOOLEAN') {
		var boolValue = eval(value) ? window.messages.boolValues.trueValue.toUpperCase() : window.messages.boolValues.falseValue.toUpperCase(); 
		$("#sensorLastObsValue").html(boolValue);
	} else {
		$("#sensorLastObsValue").html(value);
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