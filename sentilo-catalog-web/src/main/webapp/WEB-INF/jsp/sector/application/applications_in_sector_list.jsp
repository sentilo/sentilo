<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>

<c:set value="applicationInSectorTable" var="applicationTable" />

<spring:url value="/admin/application/" var="applicationDetailPrefix" />
<spring:url value="/admin/application/list/json?sectorId=${sectorId}&tableName=${applicationTable}" var="applicationsAjaxSource" />
<spring:url value="/admin/application/list/excel?tableName=${applicationTable}&sectorId=${sectorId}" var="excelSource" />
<spring:url value="/admin/sector/${sectorId}/addApplications" var="addApplicationToSectorUrl" />
<spring:url value="/admin/sector/${sectorId}/removeApplications" var="removeApplicationsFromSectorUrl" />


<script type="text/javascript">
	var tableApplicationInSector = null;
	$(document).ready(function() {	
		tableApplicationInSector =	makeTableAsync('${applicationTable}', '${applicationsAjaxSource}', '${applicationDetailPrefix}');
	});
</script>
<form:form method="post" action="${removeApplicationsFromSectorUrl}" id="removeApplicationForm" name="removeApplicationForm">
	<table class="table table-striped" id="${applicationTable}">
		<thead>
			<tr>
				<td>
					<c:choose>
						<c:when test="${showAdminControls}">
			    			<input type="checkbox" name="selectAllRows#${applicationTable}"/>
				    	</c:when>
				    	<c:otherwise>&nbsp;</c:otherwise>
				    </c:choose>
				</td>
				<td><strong><spring:message code="application.id" /> </strong></td>
				<td><strong><spring:message code="application.name" /> </strong></td>
				<td><strong><spring:message code="application.description" /> </strong></td>
				<td><strong><spring:message code="sector.provider.add.grant" /></strong></td>
			</tr>
		</thead>
		<tbody />
	</table>
</form:form>
<div class="control-group pull-left" id="excel_${applicationTable}">
	<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
		<spring:message code="button.excel" /> 
	</a>
</div>	
<div class="control-group pull-right">
	<div class="control-group pull-right">
		<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
		<c:if test="${showAdminControls}">
			<spring:message code="sector.remove.application.confirm" var="confirmDeleteMessage" />
			<a href="#" onclick="deleteSelected('removeApplicationForm', '${confirmDeleteMessage}');" class="btn btn-danger">
				<spring:message code="sector.application.remove" /> 
			</a> 
			<a href="#" onclick="showAddToSectorModalWindow('#applicationsNotInSectorModal');" class="btn btn-primary"> 
				<spring:message code="sector.application.add" />
			</a>
		</c:if>
	</div>
</div>
<%@include file="/WEB-INF/jsp/sector/application/applications_not_in_sector_list.jsp" %>