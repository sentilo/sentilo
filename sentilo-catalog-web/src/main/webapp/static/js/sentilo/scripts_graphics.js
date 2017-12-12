var data = [];
var xAxisLabels = [];
var from = null;
var to = null;
var oldToList = [];
var theFuture = null;

// Control ids
var chartNavigateLeftControlId = "chartNavigateLeft";
var chartNavigateRefreshControlId = "chartNavigateRefresh";
var chartNavigateRightControlId = "chartNavigateRight";

// Navigation data object
// Holds start & end dates, and total items number
var navData = {
	startDate: null,
	endDate: null,
	startTime: null,
	endTime: null,
	numItems: 0
};

var initChartControls = function() {
	$("#"+chartNavigateRightControlId).prop('disabled', true);
}

var yAxisMinMaxOptions = {
	min: NaN,
	max: NaN
}

/*
 * Display plot tooltips
 */

var showTooltip = function(x, y, color, contents) {
	$('<div id="tooltip" class="tooltip top in"><div class="tooltip-arrow"></div><div class="tooltip-inner">' + contents + '</div></div>').appendTo("body");
	$("#tooltip").css({ top: y - 40, left: (x - $("#tooltip").width() / 2 - 4) });
};

$.fn.UseTooltip = function () {
	 var previousPoint = null;
	 $(this).bind("plothover", function (event, pos, item) {
	 	if (item) {
	 		if (previousPoint != item.dataIndex) {
	 			previousPoint = item.dataIndex;
				$("#tooltip").remove();
	 			var x = item.datapoint[0];
	 			var y = item.datapoint[1];
	 			showTooltip(item.pageX, item.pageY, item.series.color,
	 			"<strong>" + y + "</strong>");
	 		}
	 	} else {
	 		$("#tooltip").remove();
	 		previousPoint = null;
	 	}
	});
};


/*
 * Chart xaxis options
 */
var xAxisLabelGenerator = function(x) {
	return xAxisLabels[x];
};

var xaxisOptions = {
	tickFormatter: xAxisLabelGenerator
};

/*
 * Chart yaxis options
 */
 
var yAxisLabelGenerator = function(y) {
	return y == 0 ? 'false' : 'true'; 
};

var yaxisOptions = {
	min: 0,
	max: 1,
	tickSize: 1,
	tickFormatter: yAxisLabelGenerator
};

/*
 * Chart legend options
 */
var legendOptions = {
	show: true,
    position: 'nw'
};

/*
 * Chart grid options 
 */
var gridOptions = {
	hoverable: true, 
	clickable: true, 
	tickColor: '#eee',
	borderWidth: 0
};

/**
 * Common chart initialization
 */
var initializeChart = function(placeholder) {
	$(placeholder).empty();
	data = [];
	xAxisLabels = [];
};

var initNavData = function() {
	navData.startDate = null;
	navData.endDate = null;
	navData.startTime = null;
	navData.endTime = null;
	navData.numItems = 0;
	
	initYAxisMinMaxValues();
};

var initYAxisMinMaxValues = function() {
	yAxisMinMaxOptions.min = NaN;
	yAxisMinMaxOptions.max = NaN;
};


var updateYAxisMinMaxValues = function(currentValue) {
	var theValue = Number(currentValue);
	if (theValue) {		
		if (isNaN(yAxisMinMaxOptions.max) || theValue > yAxisMinMaxOptions.max) {
			yAxisMinMaxOptions.max = theValue;
		}				
		if (isNaN(yAxisMinMaxOptions.min) || theValue < yAxisMinMaxOptions.min) {
			yAxisMinMaxOptions.min = theValue;
		}
	}
};

/*
 *
 * Makes number chart.
 */
var makeNumberChart = function(placeholder, url, label, callback) {
	
	jsonGET(url, [], function(dto) {

		initNavData();
		initializeChart(placeholder);
		
		$.each(dto.events, function(key, val) {
			data.push([key, val.value]);
			xAxisLabels.push(formatGraphTimestamp(val.timestamp));
			updateYAxisMinMaxValues(val.value);
		});
		
		// Load graph data indexes
		navData.startTime = dto.fromTime;
		navData.startDate = dto.fromTimestamp;
		navData.endTime = dto.toTime;
		navData.endDate = dto.toTimestamp;
		navData.numItems = dto.size;
		
		updateChartControlsStatus();
		
		var options = {
			legend: legendOptions,
			grid: gridOptions,
			xaxis: xaxisOptions,
			yaxis: {
				min: Math.floor(0.80 * yAxisMinMaxOptions.min),
				max: Math.ceil(1.20 * yAxisMinMaxOptions.max)
			}			
		};
		
		$.plot(placeholder, [{
			color: '#2FABE9',
			label: label,
			data: data,
			lines: {
				show: true,
				lineWidth: 3,
				fill: true,
				fillColor: {
					colors: [ { opacity: 0.08 }, { opacity: 0.1 } ] 
				}
			},
			points: {
				show: true
			},
			shadowSize: 2
		}], options);
		
		$(placeholder).UseTooltip();
		 
		// if callback exist execute it
		callback && callback(navData);
	});
};


/**
 * Makes boolean chart.
 */
var makeBooleanChart = function(placeholder, url, label, callback) {
	
	jsonGET(url, [], function(dto) {
		
		initNavData();
		initializeChart(placeholder);
		
		$.each(dto.events, function(key, val) {
			if (val.value == 'true') {
				data.push([key, 1]);
			} else {
				data.push([key, 0]);
			}
			xAxisLabels.push(formatGraphTimestamp(val.timestamp));
		});
		
		// Load graph data indexes
		navData.startTime = dto.fromTime;
		navData.startDate = dto.fromTimestamp;
		navData.endTime = dto.toTime;
		navData.endDate = dto.toTimestamp;
		navData.numItems = dto.size;
	
		updateChartControlsStatus();
		
		var options = {
			legend: legendOptions,
			grid: gridOptions,
			xaxis: xaxisOptions,
	        yaxis: yaxisOptions
		};
	
		$.plot(placeholder, [{
			color: "#2FABE9",
			data: data,
			label: label,
			bars: {
				show: true,
				align: 'center',
				barWidth: 0.5,
				fill: true,
				fillColor: {
					colors: [ { opacity: 0.08 }, { opacity: 0.1 } ] 
				}
			}
		}], options);
		
		// if callback exist execute it
		callback && callback(navData);
	});
};

/**
 * Makes text chart
 */
 
var makeTextChartLine = function(ph, timestamp, text, label) {
	ph.append('<div class="activity_text_element"><span class="label label-info">' + formatTimestamp(timestamp) + '</span>&nbsp;' + label + ':&nbsp;' + text + '</div>');
};

var makeTextChart = function(placeholder, url, label, callback) {

	var ph = $(placeholder);

	jsonGET(url, [], function(dto) {
		
		initNavData();
		initializeChart(placeholder);
		
		$.each(dto.events, function(key, val) {	
			makeTextChartLine(ph, val.timestamp, val.value, label);
		});
		
		// Load graph data indexes
		navData.startTime = dto.fromTime;
		navData.startDate = dto.fromTimestamp;
		navData.endTime = dto.toTime;
		navData.endDate = dto.toTimestamp;
		navData.numItems = dto.size;
		
		updateChartControlsStatus();
		
		// if callback exist execute it
		callback && callback(navData);
	});
};

function createChartUrl(url, from, to, limit)  {
	var newUrl = addParamToUrl(url, 'limit', limit);
	newUrl = addParamToUrl(newUrl, 'to', to);
	newUrl = addParamToUrl(newUrl, 'from', from);
	return newUrl;
}
 
function chartNavigateLeft(callback) {

	$("#"+chartNavigateRightControlId).prop('disabled', false);
	
	if (navData && navData.startTime) {
		var lastTo = oldToList[oldToList.length-1];
		if (to && to !== lastTo) {
			// Add latest to for back navigation
			oldToList.push(to);
		} else if (to && to === lastTo) {
			$("#"+chartNavigateLeftControlId).prop('disabled', true);
		}
		to = navData.startTime;
	}
	
	// Do some callback
	callback && callback();
}

function chartNavigateRight(callback) {
	
	$("#"+chartNavigateLeftControlId).prop('disabled', false);
	
	if (oldToList && oldToList.length > 0) {
		to = oldToList.pop();
	} else {
		to = theFuture;
		$("#"+chartNavigateRightControlId).prop('disabled', true);
	}
	
	// Do some callback
	callback && callback();
}

function chartNavigateRefresh(callback) {
	
	from = null;
	to = theFuture;
	oldToList = [];
	
	$("#"+chartNavigateLeftControlId).prop('disabled', false);
	$("#"+chartNavigateRightControlId).prop('disabled', true);
	
	// Do some callback
	callback && callback();
}

var updateChartControlsStatus = function () {
	if (navData.startTime === navData.endTime) {
		$("#"+chartNavigateLeftControlId).prop('disabled', true);
	}	
}