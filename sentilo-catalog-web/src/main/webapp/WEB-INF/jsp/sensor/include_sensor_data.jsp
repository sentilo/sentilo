<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_graphics.jsp"%>

<c:set var="theFuture" value="${maxSystemDateMillis}" />

<spring:url value="/admin/sensor/lastObs/${sensor.id}/" var="lastObservationsRESTURL" />
<spring:url value="/static/js/sentilo/sensor_data.js" var="sensorDataJS" />

<script type="text/javascript" src="${sensorDataJS}"></script>
<script type="text/javascript">

$(document).ready(function() {
	
	var sensorId = '${sensor.sensorId}';
	var sensorType = '${sensor.type}';
	var dataType = '${sensor.dataType}';
	var value = '${sensorLastObservation.value}';
	var formattedValue = '${sensorLastObservation.formattedValue}';
	var timestamp = '${sensorLastObservation.timestamp}';
	var sensorUnit = '${sensor.unit}';	
	var lastObsUrl = '${lastObservationsRESTURL}';
	
	initSensorDataVariables(
		sensorType,
		dataType, 
		sensorUnit, 		
		lastObsUrl
	);
	
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
		}
	};
	
	retrieveChartPanel();
	printLastSensorObservationValue(sensorId, dataType, value, formattedValue, timestamp);
	
});

</script>

<div class="row-fluid">
	<div class="span12">
		<div class="accordion" id="lastDataAccordion">
			<div class="accordion-group">
				<div class="accordion-heading">
					<a class="accordion-toggle" data-toggle="collapse" data-parent="#lastDataAccordion"
						href="#lastDataAccordionCollapse"> <i class="icon-time "></i> <spring:message
							code="sensor.observation.lastData" /> <i class="icon-chevron-down pull-right"></i> </a>
				</div>
				<div id="lastDataAccordionCollapse" class="accordion-body collapse in">
					<div class="accordion-inner">
						<div class="container-fluid">
							<div class="row-fluid">
								<div class="span3">
									<strong><spring:message code="sensor.observation.timestamp" /> </strong>
								</div>
								<div class="span9" id="current-obs-timestamp">
									<c:if test="${not empty sensorLastObservation}">
										<spring:eval expression="sensorLastObservation.timestamp" />										 									
									</c:if>
								</div>
							</div>
							<div class="row-fluid">
								<div class="span3">
									<strong><spring:message code="sensor.dataType" /> </strong>
								</div>
								<div class="span9">
									<span class="label label-info"><spring:message code="sensor.dataType.${sensor.dataType}" /> </span>
								</div>
							</div>
							<div class="row-fluid">
								<div class="span3">
									<strong><spring:message code="sensor.unit" /> </strong>
								</div>
								<div class="span9">${sensor.unit}</div>
							</div>
							<div class="row-fluid">
								<div class="span3">
									<strong><spring:message code="sensor.observation.value" /> </strong>
								</div>
								<div id="sensorLastObsValue" class="span9"></div>
							</div>
							
						</div>
					</div>
				</div>
			</div>
			<br />
			<div class="row-fluid">
				<div class="span12">
					<div class="control-group pull-right">
						<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
					</div>
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
						href="#activityAccordionCollapse"> <i class="icon-signal "></i> <spring:message
							code="sensor.observation.activity" /> <i class="icon-chevron-down pull-right"></i> </a>
				</div>
				<div id="activityAccordionCollapse" class="accordion-body collapse in">
					<div class="accordion-inner">
						<div id="plot" class="observation-graph"></div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>