<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT',provider)"/>

<c:set var="documentsTable" value="documentsTable"/>

<spring:url value="/admin/documents/list/json?entityId=${provider.id}" var="sAjaxSource" />
<spring:url value="/admin/documents/new?entityId=${provider.id}" var="addDocumentUrl" />
<spring:url value="/admin/documents/delete?entityId=${provider.id}" var="deleteDocumentsURL" />

<spring:url value="/admin/documents/doc/" var="documentDetailPrefix" />
<spring:url value="/admin/provider/list?nameTableRecover=providerTable&fromBack=true" var="backURL" />

<spring:message code="document.file.remove.sure" var="deleteDocumentConfirmMessage" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<script type="text/javascript">
	$(document).ready(function() {
		var tableProvider =	makeTableAsync('${documentsTable}', '${sAjaxSource}', '${documentDetailPrefix}');
	});
</script>

<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="documents" action="${deleteDocumentsURL}">

	<table class="table table-striped" id="${documentsTable}">
		<thead>
			<tr>
				<td>&nbsp;</td>
				<td><strong><spring:message code="document.file.name" /> </strong></td>
				<td><strong><spring:message code="document.file.description" /> </strong></td>
				<td><strong><spring:message code="document.file.contentType" /> </strong></td>
				<td><strong><spring:message code="document.file.createdBy" /> </strong></td>
				<td><strong><spring:message code="document.file.creationDate" /> </strong></td>
			</tr>
		</thead>
		<tbody />
	</table>
	<br />
	<div class="control-group pull-right">
		<div class="control-group pull-right">
			<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
			<c:if test="${showAdminControls}">
			<a href="#" onclick="deleteSelected('documents','${deleteDocumentConfirmMessage}');" class="btn btn-danger">
				<spring:message code="document.file.remove" /> 
			</a> 
			<a href="#" onclick="window.location.href='${addDocumentUrl}';" class="btn"> 
				<spring:message code="document.file.add" />
			</a>
			</c:if>
		</div>
	</div>

</form:form>