<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${not empty componentId}">
	<spring:url value="/admin/component/${componentId}/removeComponents" var="deleteURL" />
	<spring:url value="/admin/component/list/excel?tableName=${componentTable}&parentId=${componentId}" var="componentExcelSource" />
</c:if>
<c:if test="${not empty providerId}">
	<c:set var="componentTable"  value="providerDetailComponentTable" />
	<spring:url value="/admin/component/list/json?providerId=${providerId}" var="sAjaxSourceComp" />	
	<c:set value="${providerId}" var="entityId" />
	<spring:url value="/admin/component/list/excel?tableName=${componentTable}&providerId=${providerId}" var="componentExcelSource" />
</c:if>

<spring:url value="/admin/component/" var="componentDetailPrefix" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<script type="text/javascript">
$(document).ready(function() {	
	var firstColumnRenderDelegate;
	var linkToDetail = true;
	if('${providerId}'){
		linkToDetail = false;
		firstColumnRenderDelegate = function (data, type, row) {
			return '';
		}; 
	}
	makeTableAsync('${componentTable}', '${sAjaxSourceComp}', '${componentDetailPrefix}', firstColumnRenderDelegate);						    	
});

</script>

<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="components"	action="${deleteURL}">		
	<table class="table table-striped" id="${componentTable}">
		<thead>
			<tr>
				<td>
				    <c:choose>
				    	<c:when test="${empty componentId and empty providerId and showAdminControls}">
				    		<input type="checkbox" name="selectAllRows"/>
				    	</c:when>
				    	<c:otherwise>&nbsp;</c:otherwise>
				    </c:choose>								
				</td>
				<td><strong><spring:message code="component.name" /> </strong></td>
				<td><strong><spring:message code="component.description" /> </strong></td>
				<td><strong><spring:message code="component.providerId" /> </strong></td>
				<td><strong><spring:message code="component.location" /> </strong></td>
				<td><strong><spring:message code="component.type" /> </strong></td>
				<td><strong><spring:message code="component.publicAccess" /></strong></td>
				<td><strong><spring:message code="component.createdAt" /> </strong></td>
			</tr>
		</thead>
		<tbody />
	</table>	
</form:form>

<div class="control-group pull-left" id="excel_'${componentTable}'">
	<a href="#" type="button" onclick="window.location.href='${componentExcelSource}';" class="btn"> 
		<spring:message code="button.excel" /> 
	</a>
</div>


						
						
						