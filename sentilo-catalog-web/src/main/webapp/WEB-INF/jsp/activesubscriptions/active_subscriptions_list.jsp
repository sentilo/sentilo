<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<c:set var="activeSubscriptionsTable" value="activeSubscriptionsTable"/>

<spring:url value="/admin/activesubscriptions/list/json" var="sAjaxSource" />
<spring:url value="/admin/activesubscriptions/list/excel?tableName=${activeSubscriptionsTable}" var="excelSource" />

<script type="text/javascript">
	$(document).ready(function() {
		var firstColumnRenderDele = function (data, type, row) {
			return '';
		}; 
		makeTableAsync('${activeSubscriptionsTable}', '${sAjaxSource}', false, firstColumnRenderDele);
				
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
							<spring:message code="activesubscription.list.title" />
							<br />
						</h1>

						<form:form method="post">
							<table class="table table-striped" id="${activeSubscriptionsTable}">
								<thead>
									<tr>
										<td>&nbsp;</td>
										<td><strong><spring:message code="activesubscription.entityId" /> </strong></td>
										<td><strong><spring:message code="activesubscription.entityType" /> </strong></td>
										<td><strong><spring:message code="activesubscription.subscriptionType" /> </strong></td>
										<td><strong><spring:message code="activesubscription.provider" /> </strong></td>
										<td><strong><spring:message code="activesubscription.sensor" /> </strong></td>
										<td><strong><spring:message code="activesubscription.endpoint" /> </strong></td>
										<td><strong><spring:message code="activesubscription.maxRetries" /> </strong></td>				
										<td><strong><spring:message code="activesubscription.retryDelay" /> </strong></td>
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
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>				
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>