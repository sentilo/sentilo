<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:set var="sensorTable"  value="sensorTable" />
<spring:url value="/admin/sensor/list/json" var="sensorsAjaxSource" />
<spring:url value="/admin/sensor/delete" var="deleteURL" />
<spring:url value="/admin/sensor/new" var="newSensorURL" />
<spring:url value="/admin/sensor/delete" var="deleteSensorsURL" />
<spring:url value="/admin/sensor/changeAccessType" var="changeAccessTypeURL" />
<spring:url value="/admin/sensor/list/excel?tableName=${sensorTable}" var="sensorExcelSource" />


<spring:message code="sure.delete.sensor" var="deleteSensorConfirmMessage" />
<spring:message code="sure.change.accessType" var="changeAccessTypeConfirmMessage" />

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">

				<div class="row-fluid">
					<div class="span12">

						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							<spring:message code="sensor.list.title" />
							<br />
						</h1>

						<%@include file="/WEB-INF/jsp/common/include_list_sensor.jsp"%>
						
						<div class="control-group pull-right">
							<a href="#" onclick="deleteSelected('sensors','<spring:escapeBody>${deleteSensorConfirmMessage} </spring:escapeBody>');" class="btn btn-danger"> 
								<spring:message	code="sensor.delete.title" /> 
							</a> 
							<a href="#" onclick="changeAccessType('sensors', '<spring:escapeBody>${changeAccessTypeConfirmMessage}</spring:escapeBody>', 'public', '${changeAccessTypeURL}');" class="btn">							
								<spring:message code="button.accessType.change.toPublic" /> 
							</a>
							<a href="#" onclick="changeAccessType('sensors','<spring:escapeBody>${changeAccessTypeConfirmMessage}</spring:escapeBody>', 'private', '${changeAccessTypeURL}');" class="btn">
								<spring:message code="button.accessType.change.toPrivate" /> 
							</a>							
							<a href="#" onclick="window.location.href='${newSensorURL}';" class="btn"> <spring:message code="sensor.new.title" /> </a>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>
