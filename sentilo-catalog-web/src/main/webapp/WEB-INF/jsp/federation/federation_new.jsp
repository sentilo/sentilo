<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/federation/${federationConfig.id}/edit" var="actionURL" />
	<spring:message code="federation.edit.title" var="pageTitle" />
	<spring:url value="/admin/federation/${federationConfig.id}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', 'org.sentilo.web.catalog.domain.FederationConfig')"/>	
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/federation/create" var="actionURL" />
	<spring:message code="federation.new.title" var="pageTitle" />
	<spring:url value="/admin/federation/list?nameTableRecover=federationTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE', 'org.sentilo.web.catalog.domain.FederationConfig')"/>
</c:if>

<c:set var="editMode" value="${mode == 'edit' }" />

<spring:message code="select.empty" var="emptySelectMessage" javaScriptEscape="false" htmlEscape="false"/>


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
							${pageTitle}<br />
						</h1>

						<form:form method="post" modelAttribute="federationConfig" action="${actionURL}" class="form-horizontal">
							<fieldset>
								<div class="control-group">
									<form:label path="id" class="control-label">
										<spring:message code="federation.id" />
									</form:label>
									<div class="controls">
										<c:if test="${mode == 'create' }">
											<form:input path="id" />
										</c:if>
										<c:if test="${mode == 'edit' }">
											<form:input path="id" readonly="true" />
											<form:hidden path="createdAt" />
											<form:hidden path="createdBy" />
										</c:if>
										<form:errors path="id" cssClass="text-error" htmlEscape="false" />
									</div>
								</div>
								<div class="control-group">
									<form:label path="name" class="control-label">
										<spring:message code="federation.name" />
									</form:label>
									<div class="controls">
										<form:input path="name" />
										<form:errors path="name" cssClass="text-error" htmlEscape="false" />
									</div>
								</div>
								<div class="control-group">
									<form:label path="description" class="control-label">
										<spring:message code="federation.description" />
									</form:label>
									<div class="controls">
										<form:textarea path="description" />
										<form:errors path="description" cssClass="text-error" htmlEscape="false" />
									</div>
								</div>
								<input type="hidden" id="tenantId" name="tenantId" value="${tenantId}" />
								<fieldset>
									<legend>
										<spring:message code="federation.service.config.app" />
									</legend>
									<div class="control-group">
										<form:label path="appClientName" class="control-label">
											<spring:message code="federation.app_client.name" />
										</form:label>
										<div class="controls">
											<form:input path="appClientName" />
											<form:errors path="appClientName" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<div class="control-group">
										<form:label path="appClientToken" class="control-label">
											<spring:message code="federation.app_client.token" />
										</form:label>
										<div class="controls">
											<form:input path="appClientToken" />
											<form:errors path="appClientToken" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<div class="control-group">
										<form:label path="sourceEndpoint" class="control-label">
											<spring:message code="federation.service.endpoint" />
										</form:label>
										<div class="controls">
											<form:input path="sourceEndpoint" />
											<form:errors path="sourceEndpoint" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
								</fieldset>	
								
								<fieldset>
									<legend>
										<spring:message code="federation.service.contact" />
									</legend>
									
									<div class="control-group">
										<form:label path="sourceContactName" class="control-label">
											<spring:message code="federation.service.contact.name" />
										</form:label>
										<div class="controls">
											<form:input path="sourceContactName" />
											<form:errors path="sourceContactName" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
									<div class="control-group">
										<form:label path="sourceContactMail" class="control-label">
											<spring:message code="federation.service.contact.email" />
										</form:label>
										<div class="controls">
											<form:input path="sourceContactMail" />
											<form:errors path="sourceContactMail" cssClass="text-error" htmlEscape="false" />
										</div>
									</div>
								</fieldset>	
																
								<div class="control-group">
									<div class="controls">
										<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
										<c:if test="${showAdminControls}">
										<a href="#"
											onclick="$('form#federationConfig').submit();" class="btn btn-success"> <spring:message code="button.save" />
										</a>
										</c:if>
									</div>
								</div>
							</fieldset>

						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>