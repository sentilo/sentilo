//Refresh stats every 30 seconds and activity every 5 minutes
var refreshStatsMS = 1000 * 30;
var refreshActivityMS = 1000 * 60 * 5;
var datasets = [];
var choiceContainer;
var statsLink = null;
var activityLink = null;

// Get messages from JSP
var statsDevicesDctiveMsg = '';
var statsDevicesRoutersMsg = '';
var statsDevicesOthersMsg = '';
var statsEventsProcessedMsg = '';
var statsEventsOrdersMsg = '';
var statsEventsAlarms = '';
var statsEventsPersecondMsg = '';
var statsAverageRatePerdayMsg = '';
var statsMaxRateMsg = '';
var statsAccountsActiveMsg = '';
var statsAccountsProvidersMsg = '';
var statsAccountsApplicationsMsg = '';

var initUrls = function(statsUrl, activityUrl) {
	statsLink = statsUrl;
	activityLink = activityUrl;
}

var initMessages = function(statsDevicesDctive,statsDevicesRouters,statsDevicesOthers,statsEventsProcessed,statsEventsOrders,
							statsEventsAlarms,statsEventsPersecond,statsAverageRatePerday,statsMaxRate,statsAccountsActive,
							statsAccountsProviders,statsAccountsApplications) {
	statsDevicesDctiveMsg = statsDevicesDctive;
	statsDevicesRoutersMsg = statsDevicesRouters;
	statsDevicesOthersMsg = statsDevicesOthers;
	statsEventsProcessedMsg = statsEventsProcessed;
	statsEventsOrdersMsg = statsEventsOrders;
	statsEventsAlarmsMsg = statsEventsAlarms;
	statsEventsPersecondMsg = statsEventsPersecond;
	statsAverageRatePerdayMsg = statsAverageRatePerday;
	statsMaxRateMsg = statsMaxRate;
	statsAccountsActiveMsg = statsAccountsActive;
	statsAccountsProvidersMsg = statsAccountsProviders;
	statsAccountsApplicationsMsg = statsAccountsApplications;
}

var initTimes = function(theFuture) {
	to = theFuture;
}

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

var refreshStats = function(selector, line1, line2, line3, line4) {
    var stats = $(selector);
    stats.html("");
    stats.append($("<span/>").addClass("stats").html(line1));
    stats.append("<br/>");
    stats.append($("<span/>").addClass("stats-sub").html(line2));
    stats.append("<br/>");
    stats.append($("<span/>").html(line3));
    stats.append("<br/>");
    stats.append($("<span/>").html(line4));
};

var refreshDeviceStats = function(data) {
    refreshStats("#devicesStats", data.totalDevices, statsDevicesDctiveMsg, data.totalRouterDevices + ' ' + statsDevicesRoutersMsg, data.totalOtherDevices + ' ' + statsDevicesOthersMsg);	
    refreshStats("#eventsStats", data.totalEvents, statsEventsProcessedMsg, data.totalOrderEvents + ' ' + statsEventsOrdersMsg, data.totalAlarmEvents + ' ' + statsEventsAlarmsMsg);	
    refreshStats("#performanceStats", data.eventsPerSecond, statsEventsPersecondMsg, data.dailyAverageRate + ' ' + statsAverageRatePerdayMsg, data.maxRate + ' ' + statsMaxRateMsg);	
    refreshStats("#accountsStats", data.totalActiveAccounts, statsAccountsActiveMsg, data.totalProviderAccounts + ' ' + statsAccountsProvidersMsg, data.totalApplicationAccounts +  ' ' + statsAccountsApplicationsMsg);	
};

var ajaxStats = function() {
	$.getJSON(statsLink, function(data) {
		refreshDeviceStats(data);
	});
};

var ajaxActivity = function() {
	var url = createChartUrl(activityLink + '/', from, to);
	$.getJSON(url, function(data) {
		refreshActivityGraph(data);
	});
};

var initializeChart = function() {
	datasets = {
		"data": {
			label: "Data",
			data: []
		},        
		"orders": {
			label: "Orders", 
			data: []
		},
		"alarms": {
			label: "Alarms",
			data: []
		}
	};

	xAxisLabels = [];
    // hard-code color indices to prevent them from shifting as
    // countries are turned on/off
    var i = 0;
    $.each(datasets, function(key, val) {
        val.color = i;
        ++i;
    });
	    
    // insert checkboxes 
    choiceContainer = $("#choices");
    $.each(datasets, function(key, val) {
        choiceContainer.append('<input type="checkbox" style="float:left;margin-left:5px" name="' + key +
         '" checked="checked" id="id' + key + '">' +
         '<label style="float:left; margin:0 5px 5px; color:#777777;" for="id' + key + '">'
         + val.label + '</label>');
    });
    choiceContainer.find("input").click(plotAccordingToChoices);
            
    plotAccordingToChoices();
    $("#placeholder").UseTooltip();
} 

var refreshChartControlLabels = function() {
	// Update the control labels
	$("#chartToDate").text((navData.startDate !== 'undefined' && navData.startDate !== null) ? navData.startDate : '');
	$("#chartFromDate").text((navData.endDate !== 'undefined' && navData.endDate !== null) ? navData.endDate : '');
	$("#numObservations").text(navData.numItems);
}

function plotAccordingToChoices() {
    var data = [];
    $("#placeholder").empty();
    
    choiceContainer.find("input:checked").each(function () {
        var key = $(this).attr("name");
        if (key && datasets[key])
            data.push(datasets[key]);
    });
    

    if (data.length > 0) {
		$.plot($("#placeholder"), data, {
        	
			//yaxis: { min: 0, },
            //xaxis: { tickDecimals: 0 }, 
      
			series: {
				lines: { show: true,
					lineWidth: 3,
					fill: true, fillColor: { colors: [ { opacity: 0.08 }, { opacity: 0.1 } ] }
				},
				points: { show: true },
				shadowSize: 2
			},
			grid: {
				hoverable: true, 
				clickable: true, 
				tickColor: "#eee",
				borderWidth: 0
			},
			colors: ["#2FABE9","#8833aa", "#FA5833"],
			//legend: legendOptions,
			//grid: gridOptions,
			xaxis: xaxisOptions,
			//xaxis: {ticks:10, tickDecimals: 0},
			yaxis: {ticks:3, tickDecimals: 0}
        });
    }
}

function refreshActivityGraph(lastActivityLogs){
	datasets['data'].data = [];
    datasets['orders'].data = [];
    datasets['alarms'].data = [];
    xAxisLabels = [];
	
    initNavData();
    
	$.each(lastActivityLogs.events, function(index, lastActivityLog) {
        var putAlarms = lastActivityLog['putAlarms'];
        var putObservations = lastActivityLog['putObservations']; 
        var putOrders = lastActivityLog['putOrders'];	
        var timestamp = formatGraphTimestamp(lastActivityLog['timestampToString']);
        
        datasets['data'].data.push([index, putObservations]);
        datasets['orders'].data.push([index, putOrders]);
        datasets['alarms'].data.push([index, putAlarms]);
        
        xAxisLabels.push(timestamp);
    });   
	
	// Load graph data indexes
	navData.startTime = lastActivityLogs.fromTime;
	navData.startDate = lastActivityLogs.fromTimestamp;
	navData.endTime = lastActivityLogs.toTime;
	navData.endDate = lastActivityLogs.toTimestamp;
	navData.numItems = lastActivityLogs.size;
    	
	plotAccordingToChoices();
    $("#placeholder").UseTooltip();
    
	// if callback exist execute it
    refreshChartControlLabels();
}
