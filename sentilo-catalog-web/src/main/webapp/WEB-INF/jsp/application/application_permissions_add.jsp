<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

<spring:url value="/admin/permissions/application/${permissions.parentEntityId}/add" var="addPermissionURL" />
<spring:url value="/admin/application/${permissions.parentEntityId}/detail?openedTab=2" var="backURL" />

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>

			<div class="span9">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				
				<h1 class="lead">
					<spring:message code="permissions.new.title" />
				</h1>
				<form:form method="post" modelAttribute="permissions" action="${addPermissionURL}" class="form-horizontal">
					<fieldset>
						<div class="control-group">
							<form:label path="parentEntityId" class="control-label">
								<spring:message code="application.id" />
							</form:label>
							<div class="controls">
								<form:input path="parentEntityId" value="${applicationId}" readonly="true" cssClass="input-large" />
								<form:errors path="parentEntityId" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="selectedProvidersIds" class="control-label">
								<spring:message code="permissions.providers" />
							</form:label>
							<div class="controls">
								<form:select path="selectedProvidersIds" cssClass="input-large">
									<form:options items="${permissions.providers}" itemValue="value" itemLabel="label" />
								</form:select>
								<form:errors path="selectedProvidersIds" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="selectedApplicationsIds" class="control-label">
								<spring:message code="permissions.applications" />
							</form:label>
							<div class="controls">
								<form:select path="selectedApplicationsIds" cssClass="input-large">
									<form:options items="${permissions.applications}" itemValue="value" itemLabel="label" />
								</form:select>
								<form:errors path="selectedApplicationsIds" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="permissionType" class="control-label">
								<spring:message code="permissions" />
							</form:label>
							<div class="controls">
								<form:select path="permissionType" cssClass="input-large">
									<form:options items="${permissionTypes}" itemValue="value" itemLabel="label" />
								</form:select>
							</div>
						</div>
						<div class="control-group">
							<div class="controls">
								<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
								<button type="submit" class="btn">
									<spring:message code="permission.add" />
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