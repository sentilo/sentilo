<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_common_messages.jsp" %>
<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>

<c:set value="userInSectorTable" var="userTable" />
<c:set value="inSector" var="scriptSuffix" />

<spring:url value="/admin/users/" var="userDetailPrefix" />
<spring:url value="/admin/users/list/json?sectorId=${sectorId}" var="usersAjaxSource" />
<spring:url value="/admin/users/list/excel?tableName=${userTable}&sectorId=${sectorId}" var="excelSource" />
<spring:url value="/admin/sector/${sectorId}/addUsers" var="addUserToSectorUrl" />
<spring:url value="/admin/sector/${sectorId}/removeUsers" var="removeUsersFromSectorUrl" />
<spring:url value="/admin/sector/${sectorId}/addUsers" var="addUsersUrl" />
<spring:url value="/admin/sector/${sectorId}/checkIfLastSectorInUsers" var="urlCheckRemoveUsers" />

<spring:message code="sector.remove.user.confirm" var="confirmDeleteMessage" javaScriptEscape="true" />
<spring:message code="sector.user.no.sectors.admin.advise" var="userWithoutSectorsMessage" javaScriptEscape="true" />

<spring:url value="/static/js/sentilo/add_users_to_sectors.js" var="addUsersToSectorJS" />
<script type="text/javascript" src="${addUsersToSectorJS}" ></script>

<form:form method="post" action="${removeUsersFromSectorUrl}" id="removeUsersForm" name="removeUsersForm">
	<%@include file="/WEB-INF/jsp/sector/user/sector_user_list.jsp"%>
</form:form>
<div class="control-group pull-left" id="excel_${userTable}">
	<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
		<spring:message code="button.excel" /> 
	</a>
</div>	
<div class="control-group pull-right">
	<div class="control-group pull-right">
		<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
		<c:if test="${showAdminControls}">			
			<a href="#" onclick='checkSectorsAndDelete("${urlCheckRemoveUsers}", "userInSectorTable", "removeUsersForm", "${userWithoutSectorsMessage}", "${confirmDeleteMessage}");' class="btn btn-danger">
				<spring:message code="sector.user.remove" /> 
			</a> 
			<a href="#" onclick="showAddUserModalWindow();" class="btn btn-primary"> 
				<spring:message code="sector.user.add" />
			</a>
		</c:if>
	</div>
</div>
<%@include file="/WEB-INF/jsp/sector/user/users_not_sector_list.jsp" %>