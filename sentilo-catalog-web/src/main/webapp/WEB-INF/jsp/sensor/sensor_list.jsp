<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/sensor/list/json" var="sensorsAjaxSource" />
<spring:url value="/admin/sensor/delete" var="deleteURL" />
<spring:url value="/admin/sensor/new" var="newSensorURL" />
<spring:url value="/admin/sensor/delete" var="deleteSensorsURL" />

<spring:message code="sure.delete.sensor" var="deleteSensorConfirmMessage" />

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
						<%@include file="/WEB-INF/jsp/sensor/include_sensor_delete_controls.jsp"%>

					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>
