<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.Provider')"/>

<c:set var="providerTable" value="providerTable"/>

<spring:url value="/admin/provider/new" var="newProviderLink" />
<spring:url value="/admin/provider/" var="providerDetailPrefix" />
<spring:url value="/admin/provider/delete" var="deleteURL" />
<spring:url value="/admin/provider/list/excel?tableName=${providerTable}" var="excelSource" />
<spring:url value="/admin/provider/list/json" var="sAjaxSource" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<script type="text/javascript">
$(document).ready(function() {	
	
	var tableProvider =	makeTableAsync('${providerTable}', '${sAjaxSource}', '${providerDetailPrefix}');
	
});
</script>

<spring:message code="sure.delete.provider" var="deleteProviderConfirmMessage" />

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
							<spring:message code="provider.list.title" />
							<br />
						</h1>

						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="providers"
							action="${deleteURL}">

							<table class="table table-striped" id="${providerTable}">
								<thead>
									<tr>
										<td>&nbsp;
										</th>
										<td><strong><spring:message code="provider.id" /> </strong></td>
										<td><strong><spring:message code="provider.name" /> </strong></td>
										<td><strong><spring:message code="provider.description" /> </strong></td>
										<td><strong><spring:message code="provider.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${providerTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>
								
							<c:if test="${showAdminControls}">
								<div class="control-group pull-right">
									<a href="#" onclick="deleteSelected('providers','${deleteProviderConfirmMessage}');" class="btn btn-danger">
										<spring:message code="provider.delete.title" /> </a> <a href="#"
										onclick="window.location.href='${newProviderLink}';" class="btn"> <spring:message code="provider.new.title" />
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