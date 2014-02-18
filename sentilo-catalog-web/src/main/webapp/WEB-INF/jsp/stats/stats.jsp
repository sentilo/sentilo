<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_graphics.jsp"%>

<spring:url value="/stats/json" var="statsLink" />
<spring:url value="/stats/activity/json" var="activityLink" />

<script type="text/javascript">

//Refresh every ten seconds
var refreshIntervalMS = 1000 * 30;

var datasets = [];

var choiceContainer;

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
    refreshStats("#devicesStats", data.totalDevices, '<spring:message code="stats.devices.active"/>', data.totalRouterDevices + " <spring:message code="stats.devices.routers"/>", data.totalOtherDevices + " <spring:message code="stats.devices.others"/>");	
    refreshStats("#eventsStats", data.totalEvents, '<spring:message code="stats.events.processed"/>', data.totalOrderEvents + " <spring:message code="stats.events.orders"/>", data.totalAlarmEvents + " <spring:message code="stats.events.alarms"/>");	
    refreshStats("#performanceStats", data.eventsPerSecond, '<spring:message code="stats.events.persecond"/>', data.dailyAverageRate + " <spring:message code="stats.average.rate.perday"/>", data.maxRate + " <spring:message code="stats.max.rate"/>");	
    refreshStats("#accountsStats", data.totalActiveAccounts, '<spring:message code="stats.accounts.active"/>', data.totalProviderAccounts + " <spring:message code="stats.accounts.providers"/>", data.totalApplicationAccounts +  " <spring:message code="stats.accounts.applications"/>");	
};


var ajaxStats = function() {
	$.getJSON('${statsLink}', function(data) {
		refreshDeviceStats(data);
	});
};

var ajaxActivity = function() {
	$.getJSON('${activityLink}', function(data) {
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

        
    //Esta ultima funcion es la que se encarga de dibujar el grafico
    // Por lo tanto, la idea es:
    // 1. Recuperar registros del servidor
    // 2. Relennar datasets
    // 3. Pintar
    plotAccordingToChoices();
    $("#placeholder").UseTooltip();
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
	
	$.each(lastActivityLogs, function(index, lastActivityLog) {
        var alarms = lastActivityLog['alarms'];
        var observations = lastActivityLog['observations']; 
        var orders = lastActivityLog['orders'];	
        var timestamp = formatGraphTimestamp(lastActivityLog['timestampToString']);
        
        
        datasets['data'].data.push([index, observations]);
        datasets['orders'].data.push([index, orders]);
        datasets['alarms'].data.push([index, alarms]);
        
        xAxisLabels.push(timestamp);
    });   
    
    /* datasets['data'].data.push(["1", 0]); 
    datasets['data'].data.push(["2", 0]);
    datasets['data'].data.push(["3", 0]);
   
    
    datasets['orders'].data.push(["1", 0]);
    datasets['orders'].data.push(["2", 0]); 
    datasets['orders'].data.push(["3", 0]); 
    
    datasets['alarms'].data.push(["1", 0]);
    datasets['alarms'].data.push(["2", 0]);
    datasets['alarms'].data.push(["3", 0]); */ 
    
	
	plotAccordingToChoices();
    $("#placeholder").UseTooltip();
}

$(document).ready(function() {
	initializeChart();
	ajaxStats();
	ajaxActivity();
	setInterval(ajaxStats, refreshIntervalMS);
	setInterval(ajaxActivity, refreshIntervalMS);	
});
</script>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span12">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

				<h1 class="lead">
					<spring:message code="generic.title" />
					<br /> <small><spring:message code="stats.title" /> </small>
				</h1>

				<div class="row-fluid">
					<div class="span3">
						<div class="accordion" id="devicesAccordion">
							<div class="accordion-group">
								<div class="accordion-heading">
									<a class="accordion-toggle" data-toggle="collapse" data-parent="#devicesAccordion"
										href="#devicesAccordionCollapse"> <i class="icon-map-marker"></i> <spring:message code="stats.devices" />
										<i class="icon-chevron-down pull-right"></i> </a>
								</div>
								<div id="devicesAccordionCollapse" class="accordion-body collapse in">
									<div class="accordion-inner" id="devicesStats"></div>
								</div>
							</div>
						</div>
					</div>
					<div class="span3">
						<div class="accordion" id="eventsAccordion">
							<div class="accordion-group">
								<div class="accordion-heading">
									<a class="accordion-toggle" data-toggle="collapse" data-parent="#eventsAccordion"
										href="#eventsAccordionCollapse"> <i class="icon-upload"></i> <spring:message code="stats.events" /> <i
										class="icon-chevron-down pull-right"></i> </a>
								</div>
								<div id="eventsAccordionCollapse" class="accordion-body collapse in">
									<div class="accordion-inner" id="eventsStats"></div>
								</div>
							</div>
						</div>
					</div>
					<div class="span3">
						<div class="accordion" id="performanceAccordion">
							<div class="accordion-group">
								<div class="accordion-heading">
									<a class="accordion-toggle" data-toggle="collapse" data-parent="#performanceAccordion"
										href="#performanceAccordionCollapse"> <i class="icon-time"></i> <spring:message code="stats.performance" />
										<i class="icon-chevron-down pull-right"></i> </a>
								</div>
								<div id="performanceAccordionCollapse" class="accordion-body collapse in">
									<div class="accordion-inner" id="performanceStats"></div>
								</div>
							</div>
						</div>
					</div>
					<div class="span3">
						<div class="accordion" id="accountsAccordion">
							<div class="accordion-group">
								<div class="accordion-heading">
									<a class="accordion-toggle" data-toggle="collapse" data-parent="#accountsAccordion"
										href="#accountsAccordionCollapse"> <i class="icon-user"></i> <spring:message code="stats.accounts" /> <i
										class="icon-chevron-down pull-right"></i> </a>
								</div>
								<div id="accountsAccordionCollapse" class="accordion-body collapse in">
									<div class="accordion-inner" id="accountsStats"></div>
								</div>
							</div>
						</div>
					</div>
				</div>
				<div class="row-fluid">
					<div class="span12">
						<div class="accordion" id="activityAccordion">
							<div class="accordion-group">
								<div class="accordion-heading">
									<a class="accordion-toggle" data-toggle="collapse" data-parent="#activityAccordion"
										href="#activityAccordionCollapse"> <i class="icon-signal"></i> <spring:message code="stats.activity" /> <i
										class="icon-chevron-down pull-right"></i> </a>
								</div>
								<div id="activityAccordionCollapse" class="accordion-body collapse in">
									<div class="accordion-inner">
										<div id="placeholder" style="width: 95%; height: 150px; margin: 0 auto; padding: 0px; position: relative;"></div>
										<p id="choices"></p>
									</div>
								</div>
							</div>
						</div>
					</div>
				</div>

			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>