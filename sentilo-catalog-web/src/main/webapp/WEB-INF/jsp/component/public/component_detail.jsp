<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:set var="componentId" scope="request" value="${component.id}" />

<spring:url value="/admin/sensor/list/json?componentId=${component.id}" var="sensorsAjaxSource" />
<spring:url value="/admin/sensor/lastOb/" var="lastSensorObservationAjaxPrefix" />
<spring:url value="/admin/sensor/lastObs/" var="activityURLPrefix" />
<spring:url value="/admin/sensor/lastOrders/" var="ordersURLPrefix" />
<spring:url value="/admin/sensor/lastAlarms/" var="alarmsURLPrefix" />
<spring:url value="/admin/component/list?nameTableRecover=componentTable&fromBack=true" var="backURL" />

<c:set var="search" value="'" />
<c:set var="replace" value="\\'" />

<security:authorize access="isAuthenticated()">
	<c:set var="showAllSensors" value="true" />	
</security:authorize>
<!--  Only filter private sensors if user is not logged in - showAllSensors: <c:out value="${showAllSensors}"/> -->

<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_graphics.jsp"%>



<script type="text/javascript">
var refreshIntervalMS = 1000 * 30;
var handlerId = undefined;
var selectedSensor = undefined;
var dataType = 'activity';

/*
 * Updates alarm panel
 */

var makeAlarmLine = function(ph, alarm) {
	ph.append('<div class="activity_text_element"><span class="label label-important">' + formatTimestamp(alarm.timestamp) + '</span>&nbsp;' + alarm.message + '</div>');
};
 
var retrieveAlarms = function(placeholder, url) {
	var ph = $(placeholder);
	ph.empty();
	jsonGET(url, [], function(data) {
		for(var i = 0; i < data.length; i++) {
			var alarm = data[i];
			makeAlarmLine(ph, alarm);
		}
	});
};

/*
 * Updates orders panel
 */
var makeOrderLine = function(ph, order) {
	ph.append('<div class="activity_text_element"><span class="label label-success">' + formatTimestamp(order.timestamp) + '</span>&nbsp;' + order.order + '</div>');
};
 
var retrieveOrders = function(placeholder, url) {
	
	var ph = $(placeholder);
	ph.empty();
	jsonGET(url, [], function(data) {
		for(var i = 0; i < data.length; i++) {
			var order = data[i];
			makeOrderLine(ph, order);
		}
	});
};

/*
 * Updates activity panel
 */
var retrieveActivity = function(placeholder, url, label) {
	if (selectedSensor.dataType == 'BOOLEAN') {
		makeBooleanChart(placeholder, url, label);
	} else if (selectedSensor.dataType == 'TEXT') {
		makeTextChart(placeholder, url, label);
	} else {
		makeNumberChart(placeholder, url, label);
	}
};

/*
 * Refresh last sensor observation panel.
 */

var addLastSensorObservationToPanel = function(panel, data) {

	var stats = $('<div class="stats"></div>');
	var date = '<br/> <spring:message code="component.sensor.observation.lastupdated"/> <br/>';
	if (data.found) {
		date = date + formatTimestamp(data.timestamp);
					
		if (data.dataType == 'BOOLEAN') {
			data.value = eval(data.value) ? '' : 'No';
		}
		
		stats.html(data.value + ' ' + data.unit);
	} else {
		date = date + '<spring:message code="component.sensor.observation.not.found"/>';
		stats.html('<spring:message code="component.sensor.observation.not.found"/>');
	}
	panel.append(data.sensor+'<br/>'+data.sensorType+'<br/>');
	panel.append(stats);
	panel.append(date);
	panel.append($('<br/>'));
};

var retrieveLastSensorObservationPanel = function() {

	var panel = $('#lastSensorDataPanel');
	panel.empty();

	var url = '${lastSensorObservationAjaxPrefix}' + selectedSensor.id + "/";
	jsonGET(url, [], function(data) {
		addLastSensorObservationToPanel(panel, data);
	});
};

/*
 * Refresh activity, orders and alarms
 */

var retrieveRightPanel = function() {
	if (dataType === 'activity') {
		retrieveActivity('#activity_placeholder', '${activityURLPrefix}' + selectedSensor.id + "/", selectedSensor.label);
	} else if (dataType === 'orders') {
		retrieveOrders('#orders_placeholder', '${ordersURLPrefix}' + selectedSensor.id + "/") ;
	} else if (dataType === 'alarms') {
		retrieveAlarms('#alarms_placeholder', '${alarmsURLPrefix}' + selectedSensor.id + "/");
	}
}
 
/*
 * Refresh data delegate 
 */
var refreshData = function() {
	retrieveLastSensorObservationPanel();
	retrieveRightPanel();
};

/*
 * stopPreviousRefresh: Stops the timer thread if started.
 */
var stopPreviousRefresh = function() {
	if (handlerId) {
		clearInterval(handlerId);
	}
}

/*
 * restartRefreshingData: Starts the timer thread.
 */
var restartRefreshingData = function() {
	stopPreviousRefresh();
	refreshData();
	handlerId = setInterval(refreshData, refreshIntervalMS);
};

/*
 * Changes selected data type
 */
var changeDataType = function(type) {
	dataType = type;
	restartRefreshingData();
}

/*
 * Changes current selected sensor
 */
var selectSensor = function(sensor) {
	selectedSensor = sensor;
	changeDataType('activity');
}

$(document).ready(function() {
	
	var componentPath = '${component.location}';
	
	initializeMap('${component.location.centroid[1]}','${component.location.centroid[0]}', componentPath.split(','), '${componentIcon}');
	
	<c:if test="${not empty componentSensors}">
		var firstSelPublicSensor=null;
		<c:set var="alreadySet" value="${false}"/>
		<c:forEach var="sensor" items="${componentSensors}">
			<c:if test="${(showAllSensors or sensor.publicAccess) && !alreadySet}">
				<c:set var="alreadySet" value="${true}"/>  
				firstSelPublicSensor={
					'id': '${sensor.id}',			
					'label': '${fn:replace(sensor.type,search,replace)} (${sensor.unit})',
					'dataType': '${sensor.dataType}'
				};
			</c:if>
		</c:forEach>
		if (firstSelPublicSensor!=null)
			selectSensor(firstSelPublicSensor);
	</c:if>
});
</script>

<div class="container-fluid">
	<div class="content">

		<%@ include file="/WEB-INF/jsp/component/public/include_component_detail_header.jsp"%>

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