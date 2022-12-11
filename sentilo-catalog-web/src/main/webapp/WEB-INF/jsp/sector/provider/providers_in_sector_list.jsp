<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>

<c:set value="providerInSectorTable" var="providerTable" />

<spring:url value="/admin/provider/" var="providerDetailPrefix" />
<spring:url value="/admin/provider/list/json?sectorId=${sectorId}&tableName=${providerTable}" var="providersAjaxSource" />
<spring:url value="/admin/provider/list/excel?tableName=${providerTable}&sectorId=${sectorId}" var="excelSource" />
<spring:url value="/admin/sector/${sectorId}/addProviders" var="addProviderToSectorUrl" />
<spring:url value="/admin/sector/${sectorId}/removeProviders" var="removeProvidersFromSectorUrl" />



<script type="text/javascript">
	var tableProviderInSector = null;
	$(document).ready(function() {	
		tableProviderInSector =	makeTableAsync('${providerTable}', '${providersAjaxSource}', '${providerDetailPrefix}');
	});
</script>
<form:form method="post" action="${removeProvidersFromSectorUrl}" id="removeProviderForm" name="removeProviderForm">
	<table class="table table-striped" id="${providerTable}">
		<thead>
			<tr>
				<td>
					<c:choose>
						<c:when test="${showAdminControls}">
			    			<input type="checkbox" name="selectAllRows#${providerTable}"/>
				    	</c:when>
				    	<c:otherwise>&nbsp;</c:otherwise>
				    </c:choose>
				</td>
				<td><strong><spring:message code="provider.id" /> </strong></td>
				<td><strong><spring:message code="provider.name" /> </strong></td>
				<td><strong><spring:message code="provider.description" /> </strong></td>
				<td><strong><spring:message code="sector.provider.add.grant" /></strong></td>
			</tr>
		</thead>
		<tbody />
	</table>
</form:form>
<div class="control-group pull-left" id="excel_${providerTable}">
	<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
		<spring:message code="button.excel" /> 
	</a>
</div>	
<div class="control-group pull-right">
	<div class="control-group pull-right">
		<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
		<c:if test="${showAdminControls}">
			<spring:message code="sector.remove.provider.confirm" var="confirmDeleteMessage" />
			<a href="#" onclick="deleteSelected('removeProviderForm', '${confirmDeleteMessage}');" class="btn btn-danger">
				<spring:message code="sector.provider.remove" /> 
			</a> 
			<a href="#" onclick="showAddToSectorModalWindow('#providersNotInSectorModal');" class="btn btn-primary"> 
				<spring:message code="sector.provider.add" />
			</a>
		</c:if>
	</div>
</div>
<%@include file="/WEB-INF/jsp/sector/provider/providers_not_in_sector_list.jsp" %>