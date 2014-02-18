<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' && not empty sensorType.icon}">
	<c:set value="${sensorType.icon}" var="defaultSelectedIcon" />
</c:if>

<c:if test="${mode == 'create' || empty sensorType.icon}">
	<c:set value="6" var="defaultSelectedIcon" />
</c:if>

<spring:url value="/static/img" var="iconPrefix" />

<script>
var selectSensorTypeIcon = function(which) {
	$('#selectedIconImage').attr('src', '${iconPrefix}/pins' + which + ".png");
	$('#selectedIcon').val(which);
}

$(document).ready(function() {
	selectSensorTypeIcon(${defaultSelectedIcon});
});
</script>

<div class="control-group">
	<form:label path="icon" class="control-label">
		<spring:message code="sensortype.icon" />
	</form:label>
	<div class="controls">
		<form:hidden path="icon" id="selectedIcon" />
		<div class="btn-group connecta-icon-group">
			<a href="#" data-toggle="dropdown" class="btn dropdown-toggle"> &nbsp;&nbsp;&nbsp;<img
				src="${iconPrefix}/pins1.png" id="selectedIconImage"> <span class="caret"></span> </a>
			<ul class="dropdown-menu connecta-icon-dropdown-menu">
				<li><a href="#" onclick="selectSensorTypeIcon(1);"><img src="${iconPrefix}/pins1.png"> </a></li>
				<li><a href="#" onclick="selectSensorTypeIcon(2);"><img src="${iconPrefix}/pins2.png"> </a></li>
				<li><a href="#" onclick="selectSensorTypeIcon(3);"><img src="${iconPrefix}/pins3.png"> </a></li>
				<li><a href="#" onclick="selectSensorTypeIcon(4);"><img src="${iconPrefix}/pins4.png"> </a></li>
				<li><a href="#" onclick="selectSensorTypeIcon(5);"><img src="${iconPrefix}/pins5.png"> </a></li>
			</ul>
		</div>
	</div>
</div>
