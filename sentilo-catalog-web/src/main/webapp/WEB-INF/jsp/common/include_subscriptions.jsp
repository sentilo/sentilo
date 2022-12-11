<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${not empty applicationId}">
	<spring:url value="/admin/subscriptions/${applicationId}" var="sAjaxSource" />		
	<c:set value="${applicationId}" var="entityId" />
	<c:set var="subscriptionTable" value="subscriptionApplicationTable"/>
</c:if>
<c:if test="${not empty providerId}">
	<spring:url value="/admin/subscriptions/${providerId}" var="sAjaxSource" />	
	<c:set value="${providerId}" var="entityId" />
	<c:set var="subscriptionTable" value="subscriptionProviderTable"/>
</c:if>

<script type="text/javascript">
    firstColumnRenderDelegate = function (data, type, row) {
	   return '';
    };

	$(document).ready(function() {
		makeTableAsync('${subscriptionTable}', '${sAjaxSource}', false, firstColumnRenderDelegate);
	});
</script>
<form:form method="post">
	<table class="table table-striped" id="${subscriptionTable}">
		<thead>
			<tr>
			    <td>&nbsp;</td>
				<td><strong><spring:message code="subscriptions.type" /> </strong></td>
				<td><strong><spring:message code="subscriptions.provider" /> </strong></td>
				<td><strong><spring:message code="subscriptions.sensor" /> </strong></td>
				<td><strong><spring:message code="subscriptions.alert" /> </strong></td>
				<td><strong><spring:message code="subscriptions.endpoint" /> </strong></td>				
				<td><strong><spring:message code="subscriptions.maxRetries" /> </strong></td>
				<td><strong><spring:message code="subscriptions.retryDelay" /> </strong></td>
			</tr>
		</thead>
		<tbody />
	</table>
	<br />		
</form:form>
