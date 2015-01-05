<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>



<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/application/${application.id}/edit" var="actionURL" />
	<spring:message code="application.edit.title" var="pageTitle" />
	<spring:url value="/admin/application/${application.id}/detail" var="backURL" />
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/application/create" var="actionURL" />
	<spring:message code="application.new.title" var="pageTitle" />
	<spring:url value="/admin/application/list?nameTableRecover=applicationTable&fromBack=true" var="backURL" />
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
					<fieldset>
						<%@include file="/WEB-INF/jsp/common/include_input_token.jsp"%>
						<div class="control-group">
							<form:label path="id" class="control-label">
								<spring:message code="application.id" />
							</form:label>
							<div class="controls">
								<c:if test="${mode == 'create' }">
									<form:input path="id" />
								</c:if>
								<c:if test="${mode == 'edit' }">
									<form:input path="id" readonly="true" />
									<form:hidden path="createdAt" />
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
								<a href="#" onclick="$('form#application').submit();" class="btn btn-success">
								 	<spring:message code="button.save" />
								</a>
							</div>
						</div>
					</fieldset>
				</form:form>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>