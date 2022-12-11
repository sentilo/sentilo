<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/sector/${sector.id}/edit" var="actionURL" />
	<spring:message code="sector.edit.title" var="pageTitle" />
	<spring:url value="/admin/sector/${sector.id}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', 'sector')"/>
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/sector/create" var="actionURL" />
	<spring:message code="sector.new.title" var="pageTitle" />
	<spring:url value="/admin/sector/list?nameTableRecover=userTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE', 'org.sentilo.web.catalog.domain.Sector')"/>
</c:if>
<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
	function validate() {
			$('form#sector').submit();
	}
</script>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">

				<div class="row-fluid">
					<div class="span12">
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>
						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>

						<h1 class="lead">
							${pageTitle}<br />
						</h1>
						<form:form method="post" modelAttribute="sector" action="${actionURL}" class="form-horizontal">
							<c:choose>
								<c:when test="${mode == 'create' }">
									<input type="hidden" id="tenantId" name="tenantId" value="${tenantId}" />
								</c:when>
								<c:otherwise>
									<form:hidden path="tenantId" />
								</c:otherwise>
							</c:choose>							
							<spring:hasBindErrors name="sector">
								<div class="alert alert-block alert-error">
									<button type="button" class="close" data-dismiss="alert">&times;</button>
									<h5><spring:message code="error.check.form.errors" /></h5>
									<ul>
									<c:forEach var="error" items="${errors.allErrors}">
										<li class="text-error"><strong><spring:message code="${error.objectName}.${error.field}" />:</strong> <spring:message message="${error}" /></li>
									</c:forEach>
									</ul>
								</div>
							</spring:hasBindErrors>
							<div class="tabbable">
								<div class="tab-content">
									<div class="tab-pane active" id="tab1">
										<div class="control-group">
											<form:label path="id" class="control-label">
												<spring:message code="sector.id" />												
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
												<spring:message code="sector.name" />												
											</form:label>
											<div class="controls">
												<form:input path="name" />
												<form:errors path="name" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="description" class="control-label">
												<spring:message code="sector.description" />
											</form:label>
											<div class="controls">
												<form:textarea path="description" />
												<form:errors path="description" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>										
										<div class="control-group">
											<div class="controls">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<c:if test="${showAdminControls or mode == 'edit'}">
													<a href="#" onclick="validate();" class="btn btn-success"> <spring:message code="button.save" /> </a>
												</c:if>
											</div>
										</div>
									</div>							
								</div>
							</div>
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>