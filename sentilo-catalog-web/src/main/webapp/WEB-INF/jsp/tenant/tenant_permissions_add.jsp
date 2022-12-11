<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

<spring:url value="/admin/grants/${permissions.parentEntityId}/add" var="addPermissionURL" />

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>

			<div class="span9">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>

				<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">
				<h1 class="lead">
					${tenant.name}<br /> <small><spring:message code="id" /> ${tenant.id}</small>
				</h1>
				</security:authorize>

				<h1 class="lead">
					<spring:message code="tenant.permissions.to.new.title" />
				</h1>
				<br />
				
				<form:form method="post" modelAttribute="permissions" action="${addPermissionURL}" class="form-horizontal">
					<form:errors cssClass="text-error" />
					<fieldset>
						<input type="hidden" id="parentEntityId" name="parentEntityId" value="${tenantId}" />
						<div class="control-group">
							<form:label path="selectedProvidersIds" class="control-label">
								<spring:message code="tenant.permissions.providers" />
							</form:label>
							<div class="controls">
								<form:select path="selectedProvidersIds" cssClass="input-large">
									<form:options items="${permissions.providers}" itemValue="value" itemLabel="label" />
								</form:select>
								<form:errors path="selectedProvidersIds" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="selectedEntitiesIds" class="control-label">
								<spring:message code="tenant.permissions.tenants" />
							</form:label>
							<div class="controls">
								<form:select path="selectedEntitiesIds" cssClass="input-large">
									<form:options items="${permissions.entities}" itemValue="value" itemLabel="label" />
								</form:select>
								<form:errors path="selectedEntitiesIds" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="permissionType" class="control-label">
								<spring:message code="tenant.permissions.type" />
							</form:label>
							<div class="controls">
								<form:select path="permissionType" cssClass="input-large">
									<form:options items="${permissionTypes}" itemValue="value" itemLabel="label" /> 
								</form:select>
							</div>
						</div>
						<form:hidden path="visible" />
						<div class="control-group">
							<div class="controls">
								<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
								<button type="submit" class="btn">
									<spring:message code="tenant.permission.add" />
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