<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/users/${user.userName}/edit" var="actionURL" />
	<spring:message code="user.edit.title" var="pageTitle" />
	<spring:url value="/admin/users/${user.userName}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', user)"/>
	<%@include file="/WEB-INF/jsp/user/user_modalPassword.jsp"%>
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/users/create" var="actionURL" />
	<spring:message code="user.new.title" var="pageTitle" />
	<spring:url value="/admin/users/list?nameTableRecover=userTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE', 'org.sentilo.web.catalog.domain.User')"/>	
</c:if>

<script type="text/javascript">

    function showChangePasswordModalWindow() {
        showModal();
    }

	function validate() {
			$('form#user').submit();
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
						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							${pageTitle}<br />
						</h1>

						<form:form method="post" modelAttribute="user" action="${actionURL}" class="form-horizontal">

							<spring:hasBindErrors name="user">
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
								<ul class="nav nav-tabs">
									<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="component.detail.title" /></a></li>
									<li><a href="#tab2" data-toggle="tab"><spring:message code="tenant.params.title" /></a></li>
								</ul>
								<div class="tab-content">
									<div class="tab-pane active" id="tab1">
										<div class="control-group">
											<form:label path="userName" class="control-label">
												<spring:message code="user.userName" />
											</form:label>
											<div class="controls">
												<c:if test="${mode == 'create' }">
													<form:input path="userName" />
												</c:if>
												<c:if test="${mode == 'edit' }">
													<form:input path="userName" readonly="true" />
													<a href="#" onclick="showChangePasswordModalWindow();" class="btn btn-primary"> <spring:message code="user.changePassword" /> </a>
													<form:hidden path="createdAt" />
													<form:hidden path="createdBy" />
												</c:if>
												<form:errors path="userName" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="name" class="control-label">
												<spring:message code="user.name" />
											</form:label>
											<div class="controls">
												<form:input path="name" />
												<form:errors path="name" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="description" class="control-label">
												<spring:message code="user.description" />
											</form:label>
											<div class="controls">
												<form:textarea path="description" />
												<form:errors path="description" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="email" class="control-label">
												<spring:message code="user.email" />
											</form:label>
											<div class="controls">
												<form:input path="email" />
												<form:errors path="email" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="active" class="control-label">
												<spring:message code="user.active" />
											</form:label>
											<div class="controls">
											<c:if test="${mode == 'create' or showAdminControls }">
												<form:checkbox path="active" value="true"/>
												<form:errors path="active" cssClass="text-error" htmlEscape="false" />
											</c:if>
											<c:if test="${mode == 'edit' and not showAdminControls }">
												<form:checkbox path="active" value="true" disabled="true"/>
												<form:hidden path="active" />
											</c:if>
											</div>
										</div>
										<div class="control-group">
											<form:label path="active" class="control-label">
												<spring:message code="user.rols" />
											</form:label>
											<div class="controls">
                                            <c:if test="${mode == 'create' or showAdminControls }">
												<form:select path="roles" multiple="false">
													<form:option value="ADMIN" label="ADMIN" />
													<form:option value="USER" label="USER" />													
													<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">
													<form:option value="SUPER_ADMIN" label="SUPER-ADMIN" />
													</security:authorize>
												</form:select>
												<form:errors path="roles" cssClass="text-error" htmlEscape="false" />
                                            </c:if>
                                            <c:if test="${mode == 'edit' and not showAdminControls }">
												<form:select path="roles" multiple="false" disabled="true">
													<form:option value="ADMIN" label="ADMIN" />
													<form:option value="USER" label="USER" />													
													<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">
														<form:option value="SUPER_ADMIN" label="SUPER-ADMIN" />
													</security:authorize>
												</form:select>
                                                <form:hidden path="roles" />
                                            </c:if>
											</div>
										</div>
										<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">
											<div class="control-group">
												<form:label path="active" class="control-label">
													<spring:message code="user.tenant" />
												</form:label>
												<div class="controls">																				
													<c:if test="${mode == 'create' }">
														<form:select path="tenantId">
															<form:option value="">${emptySelectMessage}</form:option>
															<form:options items="${tenants}" itemValue="value" itemLabel="label" />
														</form:select>
													</c:if>
													<c:if test="${mode == 'edit' }">
														<form:input path="tenantId" readonly="true" />											
													</c:if>										
													<form:errors path="tenantId" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
										</security:authorize>
										<security:authorize access="hasRole('ROLE_ADMIN') or hasRole('ROLE_USER')">
											<input type="hidden" name="tenantId" id="tenantId" value="${tenantId}" />
										</security:authorize>
										
									</div>
									<div class="tab-pane" id="tab2">
										<c:set var="isUserConfiguration" value="true" />
										<%@include file="/WEB-INF/jsp/common/include_visual_configuration_new.jsp"%>
									</div>
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
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>