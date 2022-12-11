<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.FederationConfig')"/>

<c:set var="federationTable" value="federationTable"/>

<spring:url value="/admin/federation/delete" var="deleteURL" />
<spring:url value="/admin/federation/" var="federationDetailPrefix" />
<spring:url value="/admin/federation/new" var="newFederationURL" />
<spring:url value="/admin/federation/list/json" var="sAjaxSource" />
<spring:url value="/admin/federation/list/excel?tableName=${federationTable}" var="excelSource" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
$(document).ready(function() {		
	var table =	makeTableAsync('${federationTable}', '${sAjaxSource}', '${federationDetailPrefix}');		
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
							<spring:message code="federation.list.title" />
							<br />
						</h1>
						
						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="federations" action="${deleteURL}">

							<table class="table table-striped" id="${federationTable}">
								<thead>
									<tr>
										<td>&nbsp;</td>
										<td><strong><spring:message code="federation.id" /> </strong></td>
										<td><strong><spring:message code="federation.name" /> </strong></td>
										<td><strong><spring:message code="federation.app_client.name" /> </strong></td>										
										<td><strong><spring:message code="federation.last_sync" /> </strong></td>
										<td><strong><spring:message code="federation.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${federationTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>
							<c:if test="${showAdminControls}">		
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('federations');" class="btn btn-danger"> 
									<spring:message code="federation.delete.title" /> 
								</a> 
								<a href="#" type="button" onclick="window.location.href='${newFederationURL}';" class="btn"> 
									<spring:message code="federation.new.title" /> 
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