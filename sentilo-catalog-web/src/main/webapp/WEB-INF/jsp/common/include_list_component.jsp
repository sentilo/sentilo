<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/component/${componentId}/addComponents" var="addComponentsURL"/>
<spring:url value="/admin/component/${componentId}/removeComponents" var="removeComponentsURL" />
<spring:url value="/admin/component/" var="detailPrefix"/>

<spring:url value="/admin/component/list/json?parentId=${componentId}" var="componentsAjaxSource"/>
<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp" %>

<script type="text/javascript">
$(document).ready(function() {	
	makeTableAsync('${componentsAjaxSource}', '#componentTable', '${detailPrefix}');
});
</script>

<form:form method="post"  onkeypress="return preventEnterSubmit(event);" modelAttribute="components" action="${removeComponentsURL}">
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
<div class="control-group pull-right">
	<%@include file="/WEB-INF/jsp/common/include_input_back.jsp" %>
	<a href="#" onclick="deleteSelected('components');" class="btn btn-danger">
		<spring:message code="component.unassign.title"/>
	</a>
	<a href="#" onclick="window.location.href='${addComponentsURL}';" class="btn">
		<spring:message code="component.assign.title" />
	</a>	
</div>
