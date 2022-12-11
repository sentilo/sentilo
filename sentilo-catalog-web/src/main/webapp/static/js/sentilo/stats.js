//Refresh stats every 30 seconds and activity every 5 minutes
var refreshStatsMS = 1000 * 30;
var refreshActivityMS = 1000 * 60 * 5;
var datasets = [];
var choiceContainer;
var statsLink = null;
var activityLink = null;
var chartNav = null;

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
	chartNav = [];
	showStatisticsDataChart("#placeholder", null, null);
};

function showStatisticsDataChart(dataPanel, from, to) {
	var url = createChartUrl(activityLink + '/', from, to);
	$.getJSON(url, function(data) {
		$(dataPanel).empty();
		printStatisticsChart(data, dataPanel, {height: 300, bottomPadding: 20});
		$(dataPanel).addClass('ct-chart-centered-labels');
		
		var fromTimestamp = (data.fromTimestamp===null) ? '' : data.fromTimestamp;
		var toTimestamp = (data.toTimestamp===null) ? '' : data.toTimestamp;
		
		var iniDateMessage = (messages.chart.iniDate===null) ? 'Initial date:' : messages.chart.iniDate;
		var endDateMessage = (messages.chart.endDate===null) ? 'End date:' : messages.chart.endDate;
		
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
		
		$("#chart-controls").html($(chartControls));
		
		$("#chart-control-prev-btn").unbind('click');
		$("#chart-control-prev-btn").on('click', function(e) {
			var navActual = {
				fromTimestamp: data.fromTimestamp, 
				toTimestamp: data.toTimestamp, 
				fromTime: data.fromTime, 
				toTime: data.toTime
			};
			chartNav.push(navActual);
			showStatisticsDataChart(dataPanel, null, data.fromTime);
		});
		
		$("#chart-control-refresh-btn").unbind('click');
		$("#chart-control-refresh-btn").on('click', function(e) {
			chartNav = [];
			showStatisticsDataChart(dataPanel, null, null);
		});
		
		$("#chart-control-next-btn").unbind('click');
		$("#chart-control-next-btn").on('click', function(e) {
			var prevNav = chartNav.pop();
			var to = null;
			if (!prevNav) {
				chartNav = [];
			} else {
				to = prevNav.toTime + 1000;
			}
			showStatisticsDataChart(dataPanel, null, to);
		});
		
	});
	
}
