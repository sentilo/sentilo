<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<spring:url value="/admin/component/" var="detailPrefix" />
<spring:url value="/admin/component/new" var="newComponentLink" />
<spring:url value="/admin/component/list/json" var="sAjaxSource" />
<spring:url value="/admin/component/delete" var="deleteURL" />

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp"%>

<script type="text/javascript">

$(document).ready(function() {	
	makeTableAsync('${sAjaxSource}', '#componentTable', '${detailPrefix}');
});
</script>

<spring:message code="sure.delete.component" var="deleteComponentConfirmMessage" />

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
							<spring:message code="component.list.title" />
							<br />
						</h1>

						<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="components"
							action="${deleteURL}">
							<table class="table table-striped" id="componentTable">
								<thead>
									<tr>
										<td>&nbsp;
										</th>
										<td><strong><spring:message code="component.name" /> </strong></td>
										<td><strong><spring:message code="component.description" /> </strong></td>
										<td><strong><spring:message code="component.providerId" /> </strong></td>
										<td><strong><spring:message code="component.location" /> </strong></td>
										<td><strong><spring:message code="component.createdAt" /> </strong></td>
									</tr>
								</thead>
								<tbody />
							</table>
							<br />
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('components','<spring:escapeBody>${deleteComponentConfirmMessage}</spring:escapeBody>');" class="btn btn-danger">
									<spring:message code="component.delete.title" /> </a> <a href="#"
									onclick="window.location.href='${newComponentLink}';" class="btn"> <spring:message
										code="component.new.title" /> </a>
							</div>
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>