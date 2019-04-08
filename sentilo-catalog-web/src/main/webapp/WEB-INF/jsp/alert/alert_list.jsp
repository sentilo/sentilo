<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<c:set var="alertTable" value="alertTable"/>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.Alert')"/>

<spring:url value="/admin/alert/delete" var="deleteURL" />
<spring:url value="/admin/alert/new" var="newAlertLink" />
<spring:url value="/admin/alert/" var="alertDetailPrefix" />
<spring:url value="/admin/alert/list/excel?tableName=${alertTable}" var="excelSource" />
<spring:url value="/admin/alert/list/json" var="sAjaxSource" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
	$(document).ready(function() {
		makeTableAsync('${alertTable}', '${sAjaxSource}', '${alertDetailPrefix}');
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
							<spring:message code="alert.list.title" />
							<br />
						</h1>

						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="alerts" action="${deleteURL}">
							<table class="table table-striped" id="${alertTable}">
								<thead>
									<tr>
										<td>
										    <c:choose>
										    	<c:when test="${showAdminControls}">
										    		<input type="checkbox" name="selectAllRows"/>
										    	</c:when>
										    	<c:otherwise>&nbsp;</c:otherwise>
										    </c:choose>								
										</td>										
										<td><strong><spring:message code="alert.id" /> </strong></td>
										<td><strong><spring:message code="alert.type" /> </strong></td>
										<td><strong><spring:message code="alert.trigger" /> </strong></td>
										<td><strong><spring:message code="alert.active" /> </strong></td>
										<td><strong><spring:message code="alert.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${alertTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>	
						</form:form>
						
						<c:if test="${showAdminControls}">						
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('alerts');" class="btn btn-danger">
									<spring:message code="alert.delete.title" /> 
								</a> 
								<a href="#" type="button" onclick="window.location.href='${newAlertLink}';"	class="btn"> 
									<spring:message code="alert.new.title" /> 
								</a>
							</div>
						</c:if>
						
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>