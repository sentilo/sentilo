<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/users/${user.userName}/edit" var="actionURL" />
	<spring:message code="user.edit.title" var="pageTitle" />
	<spring:url value="/admin/users/${user.userName}/detail" var="backURL" />
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/users/create" var="actionURL" />
	<spring:message code="user.new.title" var="pageTitle" />
	<spring:url value="/admin/users/list?nameTableRecover=userTable&fromBack=true" var="backURL" />
</c:if>

<script type="text/javascript">
function validate() {
	
	var password = $('#password').val();
	var passwordRepeat = $('#passwordRepeat').val();
	
	if (password === passwordRepeat) {
		$('form#user').submit();
	} else {
		showErrorNotification('<spring:message code="user.password.notmatch"/>', '<spring:message code="user.password.notmatch"/>');
	}
}
</script>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span2">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span10">

				<div class="row-fluid">
					<div class="span12">
						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							${pageTitle}<br />
						</h1>

						<form:form method="post" modelAttribute="user" action="${actionURL}" class="form-horizontal">
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
										<form:hidden path="createdAt" />
									</c:if>
									<form:errors path="userName" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="password" class="control-label">
									<spring:message code="user.password" />
								</form:label>
								<div class="controls">
									<form:password path="password" id="password" showPassword="true" />
									<form:errors path="password" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="passwordRepeat" class="control-label">
									<spring:message code="user.password.repeat" />
								</form:label>
								<div class="controls">
									<form:password path="passwordRepeat" id="passwordRepeat" showPassword="true" />
									<form:errors path="passwordRepeat" cssClass="text-error" htmlEscape="false" />
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
									<form:checkbox path="active" value="true" />
									<form:errors path="active" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="active" class="control-label">
									<spring:message code="user.rols" />
								</form:label>
								<div class="controls">
									<form:select path="roles" multiple="true">
										<form:option value="ADMIN" label="ADMIN" />
										<form:option value="USER" label="USER" />
										<form:option value="PLATFORM" label="PLATFORM" />
									</form:select>
									<form:errors path="roles" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>

							<div class="control-group">
								<div class="controls">
									<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
									<a href="#" onclick="validate();"
										class="btn btn-success"> <spring:message code="button.save" /> </a>
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