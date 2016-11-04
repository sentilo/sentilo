<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/header.jsp"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@ include file="/WEB-INF/jsp/common/messages.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT',entity)"/>

<spring:url value="/admin/documents/create?entityId=${entityId}" var="addDocumentURL" />
<spring:url value="/admin/provider/${entityId}/detail?openedTab=5" var="backURL" />

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>

			<div class="span9">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				
				<h1 class="lead">
					<spring:message code="document.file.new.title" />
				</h1>
				
				<form:form method="post" modelAttribute="documentFile" action="${addDocumentURL}" class="form-horizontal" enctype="multipart/form-data">
					<input type="hidden" name="tenantId" id="tenantId" value="${tenantId}"  />
					<input type="hidden" name="entityId" id="entityId" value="${entityId}"  />					
					<fieldset>
						<div class="control-group">
							<form:label path="name" class="control-label">
								<spring:message code="document.file.name" />
							</form:label>
							<div class="controls">
								<form:input path="name" cssClass="input-large" />
								<form:errors path="name" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="description" class="control-label">
								<spring:message code="document.file.description" />
							</form:label>
							<div class="controls">
								<form:textarea path="description" cssClass="input-large" />
								<form:errors path="description" cssClass="text-error" htmlEscape="false" />
							</div>
						</div>
						<div class="control-group">
							<form:label path="file" class="control-label">
								<spring:message code="document.file.file" />
							</form:label>
							<div class="controls">								
								    <span class="btn btn-default btn-file">
								        <spring:message code="document.file.choose" />
								        <input type="file" id="file" name="file">
								    </span>								
								<form:errors path="file" cssClass="text-error" htmlEscape="false" />
							</div>
							
						</div>
						<div class="control-group">
							<div class="controls">
								<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
								<c:if test="${showAdminControls}">
								<button type="submit" class="btn">
									<spring:message code="document.file.add" />
								</button>
								</c:if>
							</div>
						</div>
					</fieldset>
				</form:form>
			</div>
		</div>
	</div>
</div>