// Small devices width
var smallDevicesWidth = 767;
// Number of points to display
var pointsLength;
// Labels array
var aLabels;

var isBarChart = false;
var sensorUnit;

window.messages = {
	statistics: {
		observations: 'Data',
		orders: 'Orders',
		alarms: 'Alarms'			
	}
};

function printStatisticsChart(inputData, anchor, chartOptions) {
	isBarChart = false;
	sensorUnit = undefined;
	var chartHeight = 300;
	var chartTopPadding = 30;
	var chartRightPadding = 5;
	var chartBottomPadding = 0;
	if (chartOptions !== null && chartOptions !== undefined) {
		if (chartOptions.height !== undefined) {
			chartHeight = chartOptions.height;	
		}
		if (chartOptions.topPadding !== undefined) {
			chartTopPadding = chartOptions.topPadding;	
		}
		if (chartOptions.bottomPadding !== undefined) {
			chartBottomPadding = chartOptions.bottomPadding;	
		}
		if (chartOptions.rightPadding !== undefined) {
			chartRightPadding = chartOptions.rightPadding;	
		}
	}
	
	var alarms = [];
	var observations = [];
	var orders = [];
	var timestamps = [];
	
	// To generate a tooltip not only with the data's value but also with its label (i.e. timestamp), the tooltip plugin needs to get access to the labels
	// array and get the appropriate label associated with that data.
	// Therefore, defining data value as a point (X,Y), where Y is the real value and X the position into the observations (and timestamps) array, allows tooltip 
	// plugin to display a message with both label and value information.
	
	// Finally, it's also important to define the labels array, because otherwise label data is generated with the X values (labels array overrides X content). 
	// (see getDataArray method of Chartist library)
	
	
	var i=0;
	$.each(inputData.events, function(index, lastActivityLog) {		
		// lastActivityLog contains both global counters and get/put counters. By default, activity graph display only put counters
		alarms.push({x: i , y: lastActivityLog.putAlarms});
		observations.push({x: i , y: lastActivityLog.putObservations});
		orders.push({x: i , y: lastActivityLog.putOrders});
		timestamps.push(formatLabelTimestamp(lastActivityLog.timestampToString, lastActivityLog.timestamp));
		i++;
	});
	
	// name --> serie's name
	// data --> serie's values	
	var data = {
	  labels: timestamps,
	  series: [
		  {name: window.messages.statistics.observations,   data: observations},	// data-legend=0 class="ct-series-a"
		  {name: window.messages.statistics.alarms, 	data: alarms},		    // data-legend=1 class="ct-series-b"
		  {name: window.messages.statistics.orders, 	data: orders}			// data-legend=2 class="ct-series-c"
	  ]
	};
	
	pointsLength = data.labels.length;
	aLabels = timestamps;
	
	var options = {
	  fullWidth: true,
	  height: chartHeight,
	  lineSmooth: false,
	  showArea: true,
	  showPoint: true,
	  chartPadding: {
		  top: chartTopPadding,
		  bottom: chartBottomPadding,
		  right: chartRightPadding
	  },
	  axisX: {
		  labelInterpolationFnc: labelInterpolationFunction
	  },
	  plugins: [
		  Chartist.plugins.tooltip({
			  appendToBody: true,
			  transformTooltipTextFnc: transformTooltipTextFunction
		  }),
		  Chartist.plugins.legend({ 
			  
		  })
	  ]
	};
	
	new Chartist.Line(anchor, data, options);
}

// inputData = events
function printNumericalChart(inputData, sensorType, unit, anchor, chartOptions) {
	isBarChart = false;
	sensorUnit = unit;
	var chartHeight = 300;
	var chartTopPadding = 30;
	var chartRightPadding = 5;
	var chartBottomPadding = 0;
	if (chartOptions !== null && chartOptions !== undefined) {
		if (chartOptions.height !== undefined) {
			chartHeight = chartOptions.height;	
		}
		if (chartOptions.topPadding !== undefined) {
			chartTopPadding = chartOptions.topPadding;	
		}
		if (chartOptions.bottomPadding !== undefined) {
			chartBottomPadding = chartOptions.bottomPadding;	
		}
		if (chartOptions.rightPadding !== undefined) {
			chartRightPadding = chartOptions.rightPadding;	
		}
	}
	
	var series = [];
	var labels = [];
	var i=0;
	for(var e in inputData.events) {
		var event = inputData.events[e];
		var value = event.value;
		var label = formatLabelTimestamp(event.timestamp, event.time); 		
		series.push({x: i , y: value});
		labels.push(label);
		i++;
	}
	
	var data = {
	  labels: labels,
	  series: [series]
	};
	
	pointsLength = data.labels.length;
	aLabels = labels;
	
	var options = {
	  fullWidth: true,
	  height: chartHeight,
	  lineSmooth: false,
	  showArea: true,
	  showPoint: true,
	  chartPadding: {
	    top: chartTopPadding,
	    bottom: chartBottomPadding,
	    right: chartRightPadding
	  },
	  axisX: {
		  labelInterpolationFnc: labelInterpolationFunction
	  },
	  plugins: [
		  Chartist.plugins.tooltip({
			  appendToBody: true,
			  transformTooltipTextFnc: transformTooltipTextFunction
		  }),
		  Chartist.plugins.legend({
			  legendNames: [sensorType + ((unit !== '') ? ' ('+unit+')' : '')]
		  })
	  ]
	};
	
	new Chartist.Line(anchor, data, options);
	
}

//inputData = events
function printBooleanChart(inputData, anchor, chartOptions) {
	isBarChart = true;
	sensorUnit = undefined;
	var chartHeight = 300;
	var chartTopPadding = 30;
	var chartRightPadding = 5;
	var chartBottomPadding = 0;
	if (chartOptions !== null && chartOptions !== undefined) {
		if (chartOptions.height !== undefined) {
			chartHeight = chartOptions.height;	
		}
		if (chartOptions.topPadding !== undefined) {
			chartTopPadding = chartOptions.topPadding;	
		}
		if (chartOptions.bottomPadding !== undefined) {
			chartBottomPadding = chartOptions.bottomPadding;	
		}
		if (chartOptions.rightPadding !== undefined) {
			chartRightPadding = chartOptions.rightPadding;	
		}
	}
	
	var values = [];
	var labels = [];
	var i=0;
	for(var e in inputData.events) {
		var event = inputData.events[e];
		var value = eval(event.value) ? 1 : 0;		
		var label = formatLabelTimestamp(event.timestamp, event.time);
		values.push({x: i , y: value});		
		labels.push(label);
		i++;
	}
	
	var data = {
	  labels: labels,
	  series: [values]
	};
	
	pointsLength = data.labels.length;
	aLabels = labels;
	
	var options = {
	  fullWidth: true,
	  height: chartHeight,	  
	  chartPadding: {
	    top: chartTopPadding,
	    bottom: chartBottomPadding,
	    right: chartRightPadding
	  },
	  axisX: {
		  labelInterpolationFnc: labelInterpolationFunction,
		  showGrid: false
	  },
	  axisY: {
		  high: 1,		
		  onlyInteger: true,
		  labelInterpolationFnc: valueToBooleanText
	  },plugins: [
		  Chartist.plugins.tooltip({
			  appendToBody: true,
			  transformTooltipTextFnc: transformTooltipTextFunction
		  })
	  ]
	};
	
	new Chartist.Bar(anchor, data, options);
}

function formatLabelTimestamp(timestamp, time) {   
	// For small devices, format date as hh:mm:ss
	if (window.innerWidth < smallDevicesWidth) {
		var tDate = new Date(time);
		if (tDate instanceof Date && !isNaN(tDate)) {
			var h = addZero(tDate.getHours());
		    var m = addZero(tDate.getMinutes());
		    var s = addZero(tDate.getSeconds());
		    return h + ':' + m + ':' + s;
		} 
	}else{
		return timestamp.replace(' ','\n');
	}

}

function addZero(i) {
    if (i < 10) {
        i = '0' + i;
    }
    return i;
}

function labelInterpolationFunction(value, index){
	// Hide labels if number of points to display is greater than 15
	if(pointsLength < 15){
		return value;
	}else if (pointsLength <40){
		return index % 2  === 0 ? value : null;
	}else{
		return index % 5  === 0 ? value : null;
	}
}

function transformTooltipTextFunction(tooltip) {
	var xy = tooltip.split(","); // tooltip equals "x,y"			    
	var pos = xy[0];				
	var value = !isBarChart ? xy[1] : valueToBooleanText(xy[1]); 
	if(sensorUnit){
		value = value + ' '+ sensorUnit ;
	}
	return value+'<br/>'+aLabels[pos];
}

function valueToBooleanText(value) {
  if (value == 0) {
	  return messages.boolValues.falseValue.toUpperCase();
  }
  return messages.boolValues.trueValue.toUpperCase();
}

function chartMetrics(chartElement, series, legendNames){ 
	
    var ctx = document.getElementById(chartElement);
    chart= new Chartist.Line(ctx,   {
    labels: [],
     series: JSON.parse(series)
    },
    {
    low: 0,
    showArea: true,        
    plugins: [
      Chartist.plugins.tooltip({
          appendToBody: true,
          transformTooltipTextFnc: function(tooltip) { 
            var x = tooltip.split(",");
            var t = convertLongToTime(x[0])
            return x[1] + "<br>" + t;
          }
          
      }),
      Chartist.plugins.legend({
          legendNames: legendNames              
      })
    ]
    });

    return chart;  
}
function convertTimeToLong(time){    
    var t = time.split(":");    
    var a =(+t[0] * 3600) + (+t[1] * 60) + (+t[2]); 
    return a;
}

function convertLongToTime(totalSecs){     
    var hours   = Math.floor(totalSecs / 3600);
    var minutes = Math.floor((totalSecs - (hours * 3600)) / 60);
    var seconds = totalSecs - (hours * 3600) - (minutes * 60);

    if (hours   < 10) {hours   = "0"+hours;}
    if (minutes < 10) {minutes = "0"+minutes;}
    if (seconds < 10) {seconds = "0"+seconds;}
    return hours+':'+minutes+':'+seconds;
}

// Solution to the bug about charts in tabs. When the element that contains 
// the chart is hidden when viewing the tab that contains it this is not rendered 
function chartUpdate(id){
	$(id).find('.chart').each(function(i, e) {
      e.__chartist__.update();
	}); 
}
