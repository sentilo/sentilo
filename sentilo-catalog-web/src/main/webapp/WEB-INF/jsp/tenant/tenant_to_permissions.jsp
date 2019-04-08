<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:set value="${tenantId}" var="entityId" />
<c:set value="tenants" var="modelAttribute" />
<c:set value="tenantToPermissionsTable" var="tenantToPermissionsTable" />

<spring:url value="/admin/grants/to/${tenantId}" var="sAjaxSource" />
<spring:url value="/admin/grants/${tenantId}/add" var="addPermissionURL" />
<spring:url value="/admin/grants/${tenantId}/remove" var="removePermissionURL" />
<spring:url value="/admin/grants/${tenantId}/list/excel?tableName=${tenantToPermissionsTable}" var="excelSource" />


<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<script type="text/javascript">
	$(document).ready(function() {
		var firstColumnRenderDelegate = function (data, type, row) {
			if ('${entityId}' === row[1] && '${entityId}' === row[2]) {
				return '';
			}
			return '<input type="checkbox" name="selectedIds" value="' + data +'" onclick="check(event);"/>';
		}; 
		
		var table =	makeTableAsync('${tenantToPermissionsTable}', '${sAjaxSource}', null, firstColumnRenderDelegate);
	});
</script>

<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="${modelAttribute}" action="${removePermissionURL}">

	<c:if test="${not empty tenantId}">
		<input type="hidden" name="tenantId" value="${tenantId}" />
	</c:if>

	<table class="table table-striped" id="${tenantToPermissionsTable}">
		<thead>
			<tr>
				<td>&nbsp;</td>
				<td><strong><spring:message code="tenant.permissions.organization" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.entity" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.type" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.date" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.user" /> </strong></td>
			</tr>
		</thead>
		<tbody />
	</table>
	<br />
	<%-- <div class="control-group pull-left" id="excel_${tenantToPermissionsTable}">
		<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
			<spring:message code="button.excel" /> 
		</a>
	</div> --%>	
	<div class="control-group pull-right">
		<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
		<a href="#" onclick="unassignSelected('${modelAttribute}');" class="btn btn-danger"> 
			<spring:message code="permission.remove" /> 
		</a> 
		<a href="#" onclick="window.location.href='${addPermissionURL}';" class="btn"> 
			<spring:message code="permission.add" /> 
		</a>
	</div>
</form:form>