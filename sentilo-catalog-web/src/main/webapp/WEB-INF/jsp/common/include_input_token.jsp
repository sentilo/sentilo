<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="token.regeneration.tooltip.title" var="tooltipHeader" />
<spring:message code="token.regeneration.tooltip.body" var="tooltipContent" />

<c:if test="${mode == 'edit' }">
	<div class="control-group">
		<form:label path="token" class="control-label">
			<spring:message code="token" />
		</form:label>
		<div class="controls">
			<form:input path="token" readonly="true" id="placeholder_identity_key" cssClass="input-xxlarge" />
			<a href="#" onclick='regenerateToken("#id", "${regenerateTokenUrl}", "#placeholder_identity_key", "${confirmTokenMessage}");' class="btn btn-primary"
				tooltip="${tooltipContent}" name="${tooltipHeader}" > <spring:message code="token.regeneration.button" /> </a>
			<form:errors path="token" cssClass="text-error" htmlEscape="false" />
		</div>
	</div>
</c:if>
