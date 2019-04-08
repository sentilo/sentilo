<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.User')"/>

<c:set value="userTable" var="userTable" />

<spring:url value="/admin/users/delete" var="deleteURL" />
<spring:url value="/admin/users/new" var="newUserURL" />
<spring:url value="/admin/users/list/excel?tableName=${userTable}" var="excelSource" />
<spring:url value="/admin/users/" var="userDetailPrefix" />
<spring:url value="/admin/users/list/json" var="sAjaxSource" />


<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
$(document).ready(function() {	
	var tableUser =	makeTableAsync('${userTable}', '${sAjaxSource}', '${userDetailPrefix}');
	
});
</script>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">

				<div class="row-fluid">
					<div class="span12">

						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							<spring:message code="user.list.title" />
							<br />
						</h1>


						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="users"
							action="${deleteURL}">

							<table class="table table-striped" id="${userTable}">
								<thead>
									<tr>
										<td>&nbsp;
										</th>
										<td><strong><spring:message code="user.userName" /> </strong></td>
										<td><strong><spring:message code="user.name" /> </strong></td>
										<td><strong><spring:message code="user.email" /> </strong></td>
										<td><strong><spring:message code="user.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${userTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>
							
							<c:if test="${showAdminControls}">
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('users');" class="btn btn-danger"> 
									<spring:message code="user.delete.title" /> 
								</a> 
								<a href="#" type="button" onclick="window.location.href='${newUserURL}';" class="btn"> 
									<spring:message code="user.new.title" /> 
								</a>
							</div>
							</c:if>
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>