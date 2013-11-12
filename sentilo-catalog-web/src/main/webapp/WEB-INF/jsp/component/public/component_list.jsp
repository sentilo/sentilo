<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp" %>

<spring:url value="/component/" var="detailPrefix"/>
<spring:url value="/component/list/json" var="sAjaxSource"/>

<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp" %>

<script type="text/javascript">
var firstColumnRenderDelegate = function (data, type, row) {
	return '';
}; 

$(document).ready(function() {	
	makeTableAsync('${sAjaxSource}', '#componentTable', '${detailPrefix}', false, firstColumnRenderDelegate);
});
</script>

<div class="container-fluid">
<div class="content">
<div class="row-fluid">
<div class="span12">
	
	<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp" %>
	<%@include file="/WEB-INF/jsp/common/messages.jsp" %>

	<h1 class="lead">
		<spring:message code="component.list.title"/><br/>
	</h1>

	<form:form method="post"  onkeypress="return preventEnterSubmit(event);" modelAttribute="components" action="${deleteURL}">
		<table class="table table-striped" id="componentTable">
			<thead>
				<tr>
					<td>&nbsp;</th>
					<td><strong><spring:message code="component.id"/></strong></td>
					<td><strong><spring:message code="component.name"/></strong></td>
					<td><strong><spring:message code="component.description"/></strong></td>
					<td><strong><spring:message code="component.location"/></strong></td>
					<td><strong><spring:message code="component.createdAt"/></strong></td>
				</tr>
			</thead>
			<tbody/>
		</table>
	</form:form>
</div>
</div>
</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>