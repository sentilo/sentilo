<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/component/${component.id}/addSensors" var="addSensorToComponentURL"/>

<div class="control-group pull-right">
	<%@include file="/WEB-INF/jsp/common/include_input_back.jsp" %>
	<a href="#" onclick="unassignSelected('sensors');" class="btn btn-danger">
		<spring:message code="sensor.unassign.title" />
	</a>
</div>
