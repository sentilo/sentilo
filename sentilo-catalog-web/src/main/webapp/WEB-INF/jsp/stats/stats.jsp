<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_graphics.jsp"%>

<spring:url value="/stats/json" var="statsLink" />
<spring:url value="/stats/activity/json" var="activityLink" />
<spring:url value="/static/js/sentilo/stats.js" var="statsJS" />

<c:set var="theFuture" value="${maxSystemDateMillis}" />

<script type="text/javascript" src="${statsJS}"></script>
<script type="text/javascript">

$(document).ready(function() {
	
	window.messages = {
		boolValues: {
			falseValue: '<spring:message code="false"/>',
			trueValue: '<spring:message code="true"/>'
		},
		chart: {
			iniDate: '<spring:message code="universalMap.details.sensor.chart.iniDate"/>',
			endDate: '<spring:message code="universalMap.details.sensor.chart.endDate"/>'
		},
		players: {
			timestamp: '<spring:message code="universalMap.details.players.timestamp"/>',
			filename: '<spring:message code="universalMap.details.players.filename"/>',
			ellapsed: '<spring:message code="universalMap.details.players.ellapsed"/>',
			download: '<spring:message code="universalMap.details.players.download"/>'
		},
		json: {
			timestamp: '<spring:message code="universalMap.details.json.timestamp"/>',
			expandAll: '<spring:message code="universalMap.details.json.expandAll"/>',
			collapseAll: '<spring:message code="universalMap.details.json.collapseAll"/>',
			expandLevels: '<spring:message code="universalMap.details.json.expandLevels"/>'
		},
		statistics: {
			observations: '<spring:message code="stats.events.data"/>',
			orders: '<spring:message code="stats.events.orders"/>',
			alarms: '<spring:message code="stats.events.alarms"/>'
		}
	};
	
	initUrls('${statsLink}', '${activityLink}');
	
	initMessages('<spring:message code="stats.devices.active"/>',
			 '<spring:message code="stats.devices.routers"/>',
			 '<spring:message code="stats.devices.others"/>',
			 '<spring:message code="stats.events.processed"/>',
			 '<spring:message code="stats.events.orders"/>',
			 '<spring:message code="stats.events.alarms"/>',
			 '<spring:message code="stats.events.persecond"/>',
			 '<spring:message code="stats.average.rate.perday"/>',
			 '<spring:message code="stats.max.rate"/>',
			 '<spring:message code="stats.accounts.active"/>',
			 '<spring:message code="stats.accounts.providers"/>',
			 '<spring:message code="stats.accounts.applications"/>');
	
	ajaxStats();
	ajaxActivity();
	setInterval(ajaxStats, refreshStatsMS);
	setInterval(ajaxActivity, refreshActivityMS);
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
										<div id="placeholder"></div>
										<div id="chart-controls"></div>
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