<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/component/${component.id}/edit" var="actionURL" />
	<spring:message code="component.edit.title" var="pageTitle" />
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/component/create" var="actionURL" />
	<spring:message code="component.new.title" var="pageTitle" />
</c:if>

<c:set var="editMode" value="${mode == 'edit' }" />

<spring:url value="/admin/component/list" var="backURL" />

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

				<form:form method="post" modelAttribute="component" action="${actionURL}" class="form-horizontal" autocomplete="off">
					<fieldset>
						<!-- div class="control-group">
				<form:label path="id" class="control-label">
					<spring:message code="component.id" />
				</form:label>
				<div class="controls">
					<c:if test="${mode == 'create' }">
						<form:input path="id" />
					</c:if>
					<c:if test="${mode == 'edit' }">
						<form:input path="id" readonly="true"/>
						<form:hidden path="createdAt"/>
					</c:if>
					<form:errors path="id" cssClass="text-error" htmlEscape="false" />
				</div>
			</div-->
						<c:if test="${editMode}">
							<form:hidden path="id" />
							<form:hidden path="createdAt" />
						</c:if>
						<div class="control-group">
							<form:label path="name" class="control-label">
								<spring:message code="component.name" />
							</form:label>
							<div class="controls">
								<form:input path="name" readonly="${editMode}" />
								<form:errors path="name" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="componentType" class="control-label">
								<spring:message code="component.type" />
							</form:label>
							<div class="controls">
								<form:select path="componentType">
									<form:options items="${componentTypes}" itemValue="id" itemLabel="name" />
								</form:select>
								<form:errors path="componentType" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="description" class="control-label">
								<spring:message code="component.description" />
							</form:label>
							<div class="controls">
								<form:textarea path="description" />
								<form:errors path="description" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="providerId" class="control-label">
								<spring:message code="component.providerId" />
							</form:label>
							<div class="controls">
								<form:select path="providerId" cssClass="input-large" disabled="${editMode}">
									<form:options items="${providers}" itemLabel="name" itemValue="id" />
								</form:select>
								<c:if test="${editMode}">
									<form:hidden path="providerId" />
								</c:if>
								<form:errors path="providerId" cssClass="error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="publicAccess" class="control-label">
								<spring:message code="component.publicAccess" />
							</form:label>
							<div class="controls">
								<form:checkbox path="publicAccess" />
								<form:errors path="publicAccess" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<%@include file="/WEB-INF/jsp/common/include_input_tags.jsp"%>
						<%@include file="/WEB-INF/jsp/common/include_input_location.jsp"%>
						<div class="control-group">
							<div class="controls">
								<a href="${backURL}" class="btn"> <spring:message code="button.back" /> </a> <a href="#"
									onclick="$('form#component').submit();" class="btn btn-success"> <spring:message code="button.save" /> </a>
							</div>
						</div>
					</fieldset>
				</form:form>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>