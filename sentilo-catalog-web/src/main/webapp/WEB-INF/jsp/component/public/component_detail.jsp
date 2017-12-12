<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<spring:url value="/admin/sensor/lastOb/" var="lastSensorObservationAjaxPrefix" />
<spring:url value="/admin/sensor/lastObs/" var="activityURLPrefix" />
<spring:url value="/admin/sensor/lastOrders/" var="ordersURLPrefix" />
<spring:url value="/admin/sensor/lastAlarms/" var="alarmsURLPrefix" />
<spring:url value="/admin/component/list?nameTableRecover=componentTable&fromBack=true" var="backURL" />
<spring:url value="/static/js/sentilo/component_detail.js" var="componentDetailJS" />

<c:set var="componentId" scope="request" value="${component.id}" />
<c:set var="theFuture" value="${maxSystemDateMillis}" />
<c:set var="search" value="'" />
<c:set var="replace" value="\\'" />
<c:set var="btnclose" value="${param.btnclose}" />

<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_graphics.jsp"%>

<!-- Get chart visual configuration based on component sensor / component tenant / default value -->
<c:set var="chartVisibleObservationsNumber" value="${visualConfiguration.chartVisibleObservationsNumber}" />	


<script type="text/javascript" src="${componentDetailJS}"></script>
<script type="text/javascript">

$(document).ready(function() {
	
	var componentPath = '${component.location}';
	var theFuture = '${theFuture}';
	var dataType = 'activity';
	var chartVisibleObservationsNumber = ${chartVisibleObservationsNumber};
	var lastObsUrl = '${lastSensorObservationAjaxPrefix}';
	var activityURLPrefix = '${activityURLPrefix}';
	var ordersURLPrefix = '${ordersURLPrefix}';
	var alarmsURLPrefix = '${alarmsURLPrefix}';
	
	initComponentDetailVariables(
		lastObsUrl, 
		activityURLPrefix, 
		ordersURLPrefix, 
		alarmsURLPrefix, 
		theFuture, 
		dataType, 
		chartVisibleObservationsNumber
	);
	
	initChartControls();
	
	initializeMap('${component.location.centroid[1]}','${component.location.centroid[0]}', componentPath.split(','), '${componentIcon}');
	
	<c:if test="${alternative eq 'true' && btnclose eq 'off'}">
	    // Hook to remove margin top when detail is show in a third-party window 
		$("div.content").css( "margin-top",0);
		$("div.content div").css( "margin-top",0);
	    
	    // Show accordions divs expanded	    
	    $('#photoAccordion a').click();	    
	    $('#locationAccordion a').click();
	</c:if>
	
	<c:if test="${not empty componentSensors}">
		var firstSelPublicSensor=null;
		<c:set var="alreadySet" value="${false}"/>
		<c:forEach var="sensor" items="${componentSensors}">
			<c:if test="${!alreadySet}">
				<c:set var="alreadySet" value="${true}"/>  
				firstSelPublicSensor={
					'id': '${sensor.id}',			
					'label': '${fn:replace(sensor.type,search,replace)} (${sensor.unit})',
					'dataType': '${sensor.dataType}',
					'visualConfiguration': {
						'chartVisibleObservationsNumber': '${sensor.visualConfiguration.chartVisibleObservationsNumber}'	
					}
				};
			</c:if>
		</c:forEach>
		if (firstSelPublicSensor!=null) {
			selectSensor(firstSelPublicSensor, '${lastSensorObservationAjaxPrefix}');
		} 
	</c:if>
	
	$("#chartNavigateLeft").click(function() {
		chartNavigateLeft(retrieveChartPanel);
	});
	
	$("#chartNavigateRight").click(function() {
		chartNavigateRight(retrieveChartPanel);
	});
	
	$("#chartNavigateRefresh").click(function() {
		chartNavigateRefresh(retrieveChartPanel);
	});
	
});
</script>

<div class="container-fluid">
	<div class="content">
		
		<div class="row-fluid" style="margin-top:35px; margin-bottom:20px;">
			<div class="span12" >		
				<%@ include file="/WEB-INF/jsp/component/public/include_component_detail_header.jsp"%>
			</div>
		</div>
		<div class="row-fluid">
			<div class="span9">
				<%@include file="/WEB-INF/jsp/common/include_location_map.jsp"%>
			</div>
			<div class="span3">
				<%@include file="/WEB-INF/jsp/component/public/include_component_detail_photo.jsp"%>
			</div>
		</div>

		<c:if test="${not empty componentSensors}">
			<div class="row-fluid">
				<div class="span3">
					<%@include file="/WEB-INF/jsp/component/public/include_component_detail_last_data.jsp"%>
				</div>
				<div class="span9">
					<%@include file="/WEB-INF/jsp/component/public/include_component_detail_activity.jsp"%>
				</div>
			</div>
		</c:if>


		<div class="row-fluid">
			<div class="span3">
				<strong><spring:message code="component.description" /> </strong> <br /> ${component.description}
			</div>
			<div class="row_fluid span9">
				<div class="span6">
					<strong><spring:message code="component.createdAt" />: </strong> <spring:eval expression="component.createdAt" />
				</div>
				<div class="span6">										
					<strong><spring:message code="component.location" />: </strong>
					<c:choose>	
						<c:when test="${component.mobileComponent}">
							<spring:message code="mobile" />
						</c:when> 
						<c:otherwise>
							<spring:message code="static" />
						</c:otherwise>
					</c:choose>																	
				</div>
			</div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>