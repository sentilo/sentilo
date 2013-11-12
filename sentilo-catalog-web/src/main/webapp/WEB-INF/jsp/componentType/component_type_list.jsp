<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/componenttypes/new" var="newComponentTypeURL" />
<spring:url value="/admin/componenttypes/delete" var="deleteURL" />

<spring:url value="/admin/componenttypes/list/json" var="sAjaxSource"/>
<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp" %>

<spring:url value="/admin/componenttypes/" var="detailPrefix"/>

<script type="text/javascript">
$(document).ready(function() {	
	makeTableAsync('${sAjaxSource}', '#componentTypeTable', '${detailPrefix}');
});
</script>

<div class="container-fluid">
<div class="content">
<div class="row-fluid">
<div class="span3">
	<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp" %>
</div>
<div class="span9">

<div class="row-fluid">
<div class="span12">

	<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp" %>
	<%@include file="/WEB-INF/jsp/common/messages.jsp" %>

	<h1 class="lead">
		<spring:message code="componenttype.list.title"/><br/>
	</h1>
	
	<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="componentTypes" action="${deleteURL}">
	
	<table class="table table-striped" id="componentTypeTable">
		<thead>
			<tr>
				<td>&nbsp;</th>
				<td><strong><spring:message code="componenttype.id"/></strong></td>
				<td><strong><spring:message code="componenttype.name"/></strong></td>
				<td><strong><spring:message code="componenttype.description"/></strong></td>
				<td><strong><spring:message code="componenttype.createdAt"/></strong></td>
			</tr>
		</thead>
		<tbody/>
	</table>
	<br/>
	<div class="control-group pull-right">
		<a href="#" onclick="deleteSelected('componentTypes');" class="btn btn-danger">
			<spring:message code="componenttype.delete.title"/>
		</a>
		<a href="#" type="button" onclick="window.location.href='${newComponentTypeURL}';" class="btn">
			<spring:message code="componenttype.new.title" />
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