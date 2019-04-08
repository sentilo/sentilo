<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.Application')"/>

<c:set var="applicationTable" value="applicationTable"/>

<spring:url value="/admin/application/delete" var="deleteURL" />
<spring:url value="/admin/application/" var="applicationDetailPrefix" />
<spring:url value="/admin/application/new" var="newApplicationURL" />
<spring:url value="/admin/application/list/json" var="sAjaxSource" />
<spring:url value="/admin/application/list/excel?tableName=${applicationTable}" var="excelSource" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
$(document).ready(function() {		
	var table =	makeTableAsync('${applicationTable}', '${sAjaxSource}', '${applicationDetailPrefix}');		
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
							<spring:message code="application.list.title" />
							<br />
						</h1>

						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="applications"
							action="${deleteURL}">

							<table class="table table-striped" id="${applicationTable}">
								<thead>
									<tr>
										<td>&nbsp;</td>
										<td><strong><spring:message code="application.id" /> </strong></td>
										<td><strong><spring:message code="application.name" /> </strong></td>
										<td><strong><spring:message code="application.description" /> </strong></td>
										<td><strong><spring:message code="application.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${applicationTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>	
							<c:if test="${showAdminControls}">						
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('applications');" class="btn btn-danger"> 
									<spring:message code="application.delete.title" /> 
								</a> 
								<a href="#" type="button" onclick="window.location.href='${newApplicationURL}';" class="btn"> 
									<spring:message code="application.new.title" /> 
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
