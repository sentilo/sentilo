<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
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
