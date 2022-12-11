<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="sure.change.mapVisibility" var="changeMapVisibilityConfirmMessage" />
<spring:message code="sure.change.listVisibility" var="changeListVisibilityConfirmMessage" />

<c:set value="${tenantId}" var="entityId" />
<c:set value="tenants" var="modelAttribute" />
<c:set value="tenantFromPermissionsTable" var="tenantFromPermissionsTable" />

<spring:url value="/admin/grants/${tenantId}/changeMapVisibility" var="changeMapVisibilityURL" />
<spring:url value="/admin/grants/${tenantId}/changeListVisibility" var="changeListVisibilityURL" />
<spring:url value="/admin/grants/from/${tenantId}" var="sAjaxSource" />	
<spring:url value="/admin/grants/${tenantId}/list/excel?tableName=${tenantFromPermissionsTable}" var="excelSource" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<script type="text/javascript">
	$(document).ready(function() {
		var firstColumnRenderDelegate = function (data, type, row) {
			return '<input type="checkbox" name="selectedIds" class="selectedIds" value="' + data +'"/>';
		}; 
		var table =	makeTableAsync('${tenantFromPermissionsTable}', '${sAjaxSource}', null, firstColumnRenderDelegate);
	});
</script>

<div id="noCheckModal" class="modal hide fade" tabindex="-1" role="dialog">
	<div class="modal-header">
		<button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
		<h3 id="myModalLabel"><spring:message code="tenant.permission.edit.modal.error.header" /></h3>
	</div>
	<div class="modal-body">
		<p><spring:message code="tenant.permission.edit.error.selectOne" /></p>
	</div>
	<div class="modal-footer">
	    <button class="btn" data-dismiss="modal" aria-hidden="true"><spring:message code="tenant.permission.edit.modal.button.close" /></button>
	</div>
</div>

<div id="tooManyChecksModal" class="modal hide fade" tabindex="-1" role="dialog">
	<div class="modal-header">
		<button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
		<h3 id="myModalLabel"><spring:message code="tenant.permission.edit.modal.error.header" /></h3>
	</div>
	<div class="modal-body">
		<p><spring:message code="tenant.permission.edit.error.toMany" /></p>
	</div>
	<div class="modal-footer">
	    <button class="btn" data-dismiss="modal" aria-hidden="true"><spring:message code="tenant.permission.edit.modal.button.close" /></button>
	</div>
</div>

<form:form id="editTenantPermissions" method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="${modelAttribute}" action="${changeMapVisibilityURL}">

	<c:if test="${not empty tenantId}">
		<input type="hidden" name="tenantId" value="${tenantId}" />
	</c:if>

	<table class="table table-striped" id="${tenantFromPermissionsTable}">
		<thead>
			<tr>
				<td>&nbsp;</td>
				<td><strong><spring:message code="tenant.permissions.organization" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.entity" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.type" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.date" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.visible" /> </strong></td>
				<td><strong><spring:message code="tenant.permissions.listVisible" /> </strong></td>
			</tr>
		</thead>
		<tbody />
	</table>
	<br />
	
	<%-- <div class="control-group pull-left" id="excel_${tenantFromPermissionsTable}">
		<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
			<spring:message code="button.excel" /> 
		</a>
	</div> --%>	
	<div class="control-group pull-right">
		<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
		<a href="#" onclick="changeMapVisibility('editTenantPermissions', '<spring:escapeBody>${changeMapVisibilityConfirmMessage}</spring:escapeBody>', 'public', '${changeMapVisibilityURL}');" class="btn">							
			<spring:message code="button.mapVisible.change.toPublic" /> 
		</a>
		<a href="#" onclick="changeMapVisibility('editTenantPermissions', '<spring:escapeBody>${changeMapVisibilityConfirmMessage}</spring:escapeBody>', 'private', '${changeMapVisibilityURL}');" class="btn">
			<spring:message code="button.mapVisible.change.toPrivate" /> 
		</a>
		<a href="#" onclick="changeListVisibility('editTenantPermissions', '<spring:escapeBody>${changeListVisibilityConfirmMessage}</spring:escapeBody>', 'public', '${changeListVisibilityURL}');" class="btn">							
			<spring:message code="button.listVisible.change.toPublic" /> 
		</a>
		<a href="#" onclick="changeListVisibility('editTenantPermissions', '<spring:escapeBody>${changeListVisibilityConfirmMessage}</spring:escapeBody>', 'private', '${changeListVisibilityURL}');" class="btn">
			<spring:message code="button.listVisible.change.toPrivate" /> 
		</a>
	</div>
</form:form>