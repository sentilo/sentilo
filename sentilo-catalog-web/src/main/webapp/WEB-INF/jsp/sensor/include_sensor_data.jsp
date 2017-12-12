<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_graphics.jsp"%>

<c:set var="theFuture" value="${maxSystemDateMillis}" />
<c:choose>
	<c:when test="${not empty sensor.visualConfiguration.chartVisibleObservationsNumber}">
		<!-- The sensor config -->
		<c:set var="chartObservationsNumber" value="${sensor.visualConfiguration.chartVisibleObservationsNumber}" />
	</c:when>
	<c:when test="${not empty visualConfiguration.chartVisibleObservationsNumber}">
		<!-- The tenant config -->
		<c:set var="chartObservationsNumber" value="${visualConfiguration.chartVisibleObservationsNumber}" />
	</c:when>
	<c:otherwise>
		<!-- The default value -->
		<c:set var="chartObservationsNumber" value="10" />
	</c:otherwise>
</c:choose>

<spring:url value="/admin/sensor/lastObs/${sensor.id}/" var="lastObservationsRESTURL" />
<spring:url value="/static/js/sentilo/sensor_data.js" var="sensorDataJS" />

<script type="text/javascript" src="${sensorDataJS}"></script>
<script type="text/javascript">

$(document).ready(function() {
	
	var dataType = '${sensor.dataType}';
	var value = '${sensorLastObservation.value}';
	var sensorUnit = '${sensor.unit}';
	var limit = ${chartObservationsNumber};
	var lastObsUrl = '${lastObservationsRESTURL}';
	var theFuture = '${theFuture}';
	
	initSensorDataVariables(
		dataType, 
		sensorUnit, 
		limit, 
		lastObsUrl, 
		theFuture
	);
	
	$("#chartNavigateLeft").click(function() {
		chartNavigateLeft(retrieveChartPanel);
	});
	
	$("#chartNavigateRight").click(function() {
		chartNavigateRight(retrieveChartPanel);
	});
	
	$("#chartNavigateRefresh").click(function() {
		chartNavigateRefresh(retrieveChartPanel);
	});
	
	initChartControls();
	retrieveChartPanel();
	printLastSensorObservationValue(dataType, value);
	
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
								<div class="span4">
									<strong><spring:message code="sensor.observation.timestamp" /> </strong>
								</div>
								<div class="span8">
									<c:if test="${not empty sensorLastObservation}">
										${fn:replace(sensorLastObservation.timestamp,'T', ' ')} 										
									</c:if>
								</div>
							</div>
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.dataType" /> </strong>
								</div>
								<div class="span8">
									<span class="label label-info"><spring:message code="sensor.dataType.${sensor.dataType}" /> </span>
								</div>
							</div>
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.unit" /> </strong>
								</div>
								<div class="span8">${sensor.unit}</div>
							</div>
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.observation.value" /> </strong>
								</div>
								<div id="sensorLastObsValue" class="span8"></div>
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
						<div class="observation-graph" id="plot"></div>
						<hr />
						<%@ include file="/WEB-INF/jsp/common/include_chart_controls.jsp" %>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>