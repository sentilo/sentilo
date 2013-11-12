<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="control-group pull-right">
	<a href="#" onclick="deleteSelected('sensors','${deleteSensorConfirmMessage}');" class="btn btn-danger">
		<spring:message code="sensor.delete.title"/>
	</a>
	<a href="#" onclick="window.location.href='${newSensorURL}';" class="btn">
		<spring:message code="sensor.new.title" />
	</a>
</div>
