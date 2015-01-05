<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${not empty applicationId}">
	<spring:url value="/admin/subscriptions/application/${applicationId}" var="sAjaxSource" />
	<c:set value="${applicationId}" var="entityId" />
	<spring:url value="/application/" var="detailPrefix" />
	<c:set var="subscriptionTable" value="subscriptionApplicationTable"/>
</c:if>
<c:if test="${not empty providerId}">
	<spring:url value="/admin/subscriptions/provider/${providerId}" var="sAjaxSource" />
	<spring:url value="/provider/" var="detailPrefix" />
	<c:set value="${providerId}" var="entityId" />
	<c:set var="subscriptionTable" value="subscriptionProviderTable"/>
</c:if>

<script type="text/javascript">
	$(document).ready(function() {
		var firstColumnRenderDele = function (data, type, row) {
			return data;
		}; 
		makeTableAsync('${subscriptionTable}', '${sAjaxSource}', false, firstColumnRenderDele,false);
		$('#'+'${subscriptionTable}'+'_filter').hide();
				
		});
</script>
<form:form method="post">
	<table class="table table-striped" id="${subscriptionTable}">
		<thead>
			<tr>
				<td><strong><spring:message code="application.type" /> </strong></td>
				<td><strong><spring:message code="application.provider" /> </strong></td>
				<td><strong><spring:message code="application.sensor" /> </strong></td>
				<td><strong><spring:message code="application.alarm" /> </strong></td>
				<td><strong><spring:message code="application.endpoint" /> </strong></td>				
			</tr>
		</thead>
		<tbody />
	</table>
	<br />		
</form:form>
