<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>

			<div class="span9">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>

				<c:set value="${componentSensors.componentId}" var="componentId" />
				<spring:url value="/admin/component/${componentId}/addSensors" var="addSensorsURL" />

				<h1 class="lead">
					<spring:message code="sensor.assign.title" />
					<br />
				</h1>

				<form:form method="post" modelAttribute="componentSensors" action="${addSensorsURL}" class="form-horizontal">
					<fieldset>
						<div class="control-group">
							<form:label path="componentId" class="control-label">
								<spring:message code="component.id" />
							</form:label>
							<div class="controls">
								<form:input path="componentId" value="${componentId}" readonly="true" />
								<form:errors path="componentId" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="selectedIds" class="control-label">
								<spring:message code="component.assign.title" />
							</form:label>
							<div class="controls">
								<form:select path="selectedIds">
									<form:options items="${componentSensors.sensors}" itemLabel="sensorId" itemValue="id" />
								</form:select>
								<form:errors path="selectedIds" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<div class="controls">
								<button type="submit" class="btn">
									<spring:message code="sensor.assign.title" />
								</button>
							</div>
						</div>
					</fieldset>
				</form:form>

			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>