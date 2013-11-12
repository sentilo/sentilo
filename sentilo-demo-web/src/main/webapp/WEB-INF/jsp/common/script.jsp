<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="taglibs.jsp"%>

<script type="text/javascript">
function formatTimestamp(timestamp) {
	var d = new Date(timestamp);
	return d.getDate() + '/' + (d.getMonth() + 1) + '/' + d.getFullYear() + '&nbsp;' + d.getHours() + ':' + d.getMinutes() + ':' + d.getSeconds();
} 
function formatGraphTimestamp(timestamp) {
	var d = new Date(timestamp);
	return d.getDate() + '/' + (d.getMonth() + 1) + '/' + d.getFullYear() + '<br/>' + d.getHours() + ':' + d.getMinutes() + ':' + d.getSeconds();
} 
</script>

<spring:url value="/static/js/jquery-1.9.0.js" var="jqueryJS"/>
<script type="text/javascript" src="${jqueryJS}"></script>
<spring:url value="/static/js/bootstrap.min.js" var="bootstrapJS"/>
<script type="text/javascript" src="${bootstrapJS}"></script>
<spring:url value="/static/js/bootbox.js" var="bootboxJS"/>
<script type="text/javascript" src="${bootboxJS}"></script>