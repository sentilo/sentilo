<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">

	<c:if test="${not empty provider.id}">
		<spring:url value="/api/identity/PROVIDER/${provider.id}/renew" var="renewIdentityKeyURL" />
	</c:if>
	<c:if test="${not empty application.id}">
		<spring:url value="/api/identity/APPLICATION/${application.id}/renew" var="renewIdentityKeyURL" />
	</c:if>

	<script type="text/javascript">
	function renewToken() {
		$.getJSON('${renewIdentityKeyURL}', function(token) {
			$('#placeholder_identity_key').val(token.value);
		});
	}
	</script>

	<div class="control-group">
		<form:label path="token" class="control-label">
			<spring:message code="token" />
		</form:label>
		<div class="controls">
			<form:input path="token" readonly="true" id="placeholder_identity_key" cssClass="input-xxlarge" />
			<form:errors path="token" cssClass="text-error" htmlEscape="false" />
		</div>
	</div>
</c:if>
