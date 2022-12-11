<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="container-fluid">
	<div class="content">
		<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
		<div class="row-fluid">
			<div style="text-align: center; margin: 0 auto;">
				<spring:url value="/j_spring_security_check" var="springSecurityCheck" />
				<form action="${springSecurityCheck}" method="post">
					<fieldset>
						<legend>
							<spring:message code="login.instructions" />
						</legend>
						<c:if test="${not empty param.error and not empty _CUSTOM_AUTH_ERROR_CODE}">
							<div class="alert alert-error">
								<button type="button" class="close" data-dismiss="alert">&times;</button>
								<spring:message code="${_CUSTOM_AUTH_ERROR_CODE}"/>															
							</div>
						</c:if>												
						<div class="control-group">
							<label for="j_username" class="control-label"> <spring:message code="login.username" /> </label>
							<div class="controls">
								<input id="j_username" name="j_username" type="text" autofocus="autofocus" />
							</div>
						</div>
						<div class="control-group">
							<label for="j_password" class="control-label"> <spring:message code="login.password" /> </label>
							<div class="controls">
								<input id="j_password" name="j_password" type="password" />
							</div>
						</div>
						<div class="control-group">
							<div class="controls">
								<button type="submit" class="btn">
									<spring:message code="login.login" />
								</button>
							</div>
						</div>
						<input type="hidden"
							   name="${_csrf.parameterName}"
							   value="${_csrf.token}"/>
					</fieldset>
				</form>
			</div>			
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>