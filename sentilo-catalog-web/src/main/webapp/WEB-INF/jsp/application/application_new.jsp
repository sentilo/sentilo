<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="entity.api.rest_https.tooltip" var="apiRestHttpsTooltip"/>
<spring:message code="entity.api.input_quota.tooltip" var="apiInputQuotaTooltip"/>
<spring:message code="entity.api.output_quota.tooltip" var="apiOutputQuotaTooltip"/>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/application/${application.id}/edit" var="actionURL" />
	<spring:message code="application.edit.title" var="pageTitle" />
	<spring:url value="/admin/application/${application.id}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', application)"/>
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/application/create" var="actionURL" />
	<spring:message code="application.new.title" var="pageTitle" />
	<spring:url value="/admin/application/list?nameTableRecover=applicationTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE', 'org.sentilo.web.catalog.domain.Application')"/>
</c:if>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">
				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

				<h1 class="lead">
					${pageTitle}<br />
				</h1>

				<form:form method="post" modelAttribute="application" action="${actionURL}" class="form-horizontal">
					<c:choose>
						<c:when test="${mode == 'create' }">
							<input type="hidden" id="tenantId" name="tenantId" value="${tenantId}" />
						</c:when>
						<c:otherwise>
							<form:hidden path="tenantId" />
						</c:otherwise>
					</c:choose>
					<fieldset>
						<%@include file="/WEB-INF/jsp/common/include_input_token.jsp"%>
						<div class="control-group">
							<form:label path="id" class="control-label">
								<spring:message code="application.id" />
							</form:label>
							<div class="controls">
								<c:if test="${mode == 'create' }">
									<c:if test="${multitenantIsEnabled}">
										<span class="label">${tenantId}@</span>
									</c:if>
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
								<spring:message code="application.name" />
							</form:label>
							<div class="controls">
								<form:input path="name" />
								<form:errors path="name" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="description" class="control-label">
								<spring:message code="application.description" />
							</form:label>
							<div class="controls">
								<form:textarea path="description" />
								<form:errors path="description" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="restHttps" class="control-label">
								<spring:message code="application.restHttps" />
							</form:label>
							<div class="controls">
								<form:checkbox path="restHttps" tooltip="${apiRestHttpsTooltip}"/>
								<form:errors path="restHttps" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="apiInputQuota" class="control-label">
								<spring:message code="entity.apiInputQuota" />
							</form:label>
							<div class="controls">
								<form:input path="apiInputQuota" tooltip="${apiInputQuotaTooltip}"/>								
								<form:errors path="apiInputQuota" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="apiOutputQuota" class="control-label">
								<spring:message code="entity.apiOutputQuota" />
							</form:label>
							<div class="controls">
								<form:input path="apiOutputQuota" tooltip="${apiOutputQuotaTooltip}"/>
								<form:errors path="apiOutputQuota" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>						
						<div class="control-group">
							<form:label path="email" class="control-label">
								<spring:message code="application.email" />
							</form:label>
							<div class="controls">
								<form:input path="email" />
								<form:errors path="email" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>

						<div class="control-group">
							<div class="controls">
								<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
								<c:if test="${showAdminControls}">
								<a href="#" onclick="$('form#application').submit();" class="btn btn-success">
								 	<spring:message code="button.save" />
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
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>