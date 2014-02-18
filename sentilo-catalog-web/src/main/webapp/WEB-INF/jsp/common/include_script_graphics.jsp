<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/static/js/jquery.flot.js" var="jqueryFlotJS" />
<script type="text/javascript" src="${jqueryFlotJS}"></script>
<spring:url value="/static/js/jquery.flot.categories.js" var="jqueryFlotCategoriesJS" />
<script type="text/javascript" src="${jqueryFlotCategoriesJS}"></script>
<spring:url value="/static/js/jquery.flot.resize.js" var="jqueryFlotResizeJS" />
<script type="text/javascript" src="${jqueryFlotResizeJS}"></script>


<script type="text/javascript">

var data = [];
var xAxisLabels = [];

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

/*
 * Makes number chart.
 */
var makeNumberChart = function(placeholder, url, label) {

	jsonGET(url, [], function(observations) {

		initializeChart(placeholder);
		$.each(observations, function(key, val) {
			data.push([key, val.value]);
			xAxisLabels.push(formatGraphTimestamp(val.timestamp));
		});

		var options = {
			legend: legendOptions,
			grid: gridOptions,
			xaxis: xaxisOptions
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
	});
};


/**
 * Makes boolean chart.
 */
var makeBooleanChart = function(placeholder, url, label) {

	jsonGET(url, [], function(observations) {

		initializeChart(placeholder);
		$.each(observations, function(key, val) {
			if (val.value == 'true') {
				data.push([key, 1]);
			} else {
				data.push([key, 0]);
			}
			xAxisLabels.push(formatGraphTimestamp(val.timestamp));
		});
	
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
	});
};

/**
 * Makes text chart
 */
 
var makeTextChartLine = function(ph, timestamp, text, label) {
	ph.append('<div class="activity_text_element"><span class="label label-info">' + formatTimestamp(timestamp) + '</span>&nbsp;' + label + ':&nbsp;' + text + '</div>');
};

var makeTextChart = function(placeholder, url, label) {

	var ph = $(placeholder);

	jsonGET(url, [], function(observations) {
		initializeChart(placeholder);
		$.each(observations, function(key, val) {	
			makeTextChartLine(ph, val.timestamp, val.value, label);
		});
	});
};
</script>