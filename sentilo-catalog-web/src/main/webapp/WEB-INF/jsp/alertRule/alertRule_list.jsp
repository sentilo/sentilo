<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<c:set var="alertRuleTable" value="alertRuleTable"/>
<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.AlertRule')"/>

<spring:url value="/admin/alertRule/delete" var="deleteURL" />
<spring:url value="/admin/alertRule/new" var="newRuleURL" />
<spring:url value="/admin/alertRule/" var="alertRuleDetailPrefix" />
<spring:url value="/admin/alertRule/list/excel?tableName=${alertRuleTable}" var="excelSource" />
<spring:url value="/admin/alertRule/list/json" var="sAjaxSource" />


<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>
<script type="text/javascript">
	$(document).ready(function() {
		makeTableAsync('${alertRuleTable}', '${sAjaxSource}', '${alertRuleDetailPrefix}');
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
							<spring:message code="alertrule.list.title" />
							<br />
						</h1>

						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="alertRules" action="${deleteURL}">
							<table class="table table-striped" id="${alertRuleTable}">
								<thead>
									<tr>
										<td>&nbsp;</td>
										<td><strong><spring:message code="alertrule.name" /> </strong></td>
										<td><strong><spring:message code="alertrule.providerId" /> </strong></td>
										<td><strong><spring:message code="alertrule.componentType" /> </strong></td>
										<td><strong><spring:message code="alertrule.sensorType" /> </strong></td>
										<td><strong><spring:message code="alertrule.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-left" id="excel_${alertRuleTable}">
								<a href="#" type="button" onclick="window.location.href='${excelSource}';" class="btn"> 
									<spring:message code="button.excel" /> 
								</a>
							</div>	
							
							<c:if test="${showAdminControls}">						
								<div class="control-group pull-right">
									<a href="#" onclick="deleteSelected('alertRules');" class="btn btn-danger">
										<spring:message code="alertrule.delete.title" /> 
									</a> 
									<a href="#" type="button" onclick="window.location.href='${newRuleURL}';"	class="btn"> 
										<spring:message code="alertrule.new.title" /> 
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