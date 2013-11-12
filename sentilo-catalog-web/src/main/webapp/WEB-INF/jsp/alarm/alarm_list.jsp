<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/alarm/delete" var="deleteURL" />
<spring:url value="/admin/alarm/new" var="newAlarmLink" />
<spring:url value="/admin/alarm/" var="detailPrefix"/>

<spring:url value="/admin/alarm/list/json" var="sAjaxSource"/>
<%@include file="/WEB-INF/jsp/common/include_script_tables.jsp" %>

<script type="text/javascript">
$(document).ready(function() {	
	makeTableAsync('${sAjaxSource}', '#alarmTable', '${detailPrefix}');
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
		<spring:message code="alarm.list.title"/><br/>
	</h1>

	<form:form method="post" onkeypress="return preventEnterSubmit(event);" modelAttribute="alarms" action="${deleteURL}">	
		<table class="table table-striped" id="alarmTable">
		<thead>
			<tr>
				<td>&nbsp;</td>
				<td><strong><spring:message code="alarm.id"/></strong></td>
				<td><strong><spring:message code="alarm.name"/></strong></td>
				<td><strong><spring:message code="alarm.type"/></strong></td>
				<td><strong><spring:message code="alarm.createdAt"/></strong></td>
			</tr>
		</thead>
		<tbody/>
		</table>
		<br/>
		<div class="control-group pull-right">
			<a href="#" onclick="deleteSelected('alarms');" class="btn btn-danger">
				<spring:message code="alarm.delete.title"/>
			</a>
			<a href="#" type="button" onclick="window.location.href='${newAlarmLink}';" class="btn">
				<spring:message code="alarm.new.title" />
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