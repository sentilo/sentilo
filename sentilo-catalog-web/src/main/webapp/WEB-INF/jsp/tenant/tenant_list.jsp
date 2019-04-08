<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.Tenant')"/>

<c:set var="tenantTable" value="tenantTable"/>

<spring:url value="/admin/tenant/delete" var="deleteURL" />
<spring:url value="/admin/tenant/" var="tenantDetailPrefix" />
<spring:url value="/admin/tenant/new" var="newTenantURL" />
<spring:url value="/admin/tenant/list/json" var="sAjaxSource" />
<spring:url value="/admin/tenant/list/excel?tableName=${tenantTable}" var="excelSource" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
$(document).ready(function() {		
	var table =	makeTableAsync('${tenantTable}', '${sAjaxSource}', '${tenantDetailPrefix}');		
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
							<spring:message code="tenant.list.title" />
							<br />
						</h1>
						
						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="tenants" action="${deleteURL}">

							<table class="table table-striped" id="${tenantTable}">
								<thead>
									<tr>
										<td>&nbsp;</td>
										<td><strong><spring:message code="tenant.id" /> </strong></td>
										<td><strong><spring:message code="tenant.name" /> </strong></td>
										<td><strong><spring:message code="tenant.description" /> </strong></td>
										<td><strong><spring:message code="tenant.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${tenantTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>
							<c:if test="${showAdminControls}">		
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('tenants');" class="btn btn-danger"> 
									<spring:message code="tenant.delete.title" /> 
								</a> 
								<a href="#" type="button" onclick="window.location.href='${newTenantURL}';" class="btn"> 
									<spring:message code="tenant.new.title" /> 
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