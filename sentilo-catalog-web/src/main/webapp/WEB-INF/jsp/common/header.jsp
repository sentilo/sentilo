<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:set var="alternative" value="${param.alternative}" />
<c:set var="testPage" value="${param.testPage}" />
<c:set var="contextPath" value="${pageContext.request.contextPath}" />

<spring:url value="/static/css/bootstrap.min.css" var="bootstrapCSS" />
<spring:url value="/static/css/bootstrap-responsive.min.css" var="bootstrapResponsiveCSS" />
<spring:url value="/static/css/styles.css" var="catalogCSS" />
<spring:url value="/static/css/datepicker.css" var="datePickerCSS" />
<spring:url value="/static/css/daterangepicker.css" var="dateRangePickerCSS" />
<spring:url value="/static/css/jquery.dataTables.css" var="jqueryDataTablesCSS" />
<spring:url value="/static/css/DT_bootstrap.css" var="dataTablesBootstrapCSS" />
<spring:url value="/static/css/font-awesome.min.css" var="awesomeFontCSS" />
<spring:url value="/static/css/font-awesome-ie7.min.css" var="awesomeFontIE7CSS" />
<spring:url value="/static/css/jquery.pnotify.default.css" var="jqueryPinesNotifyCSS" />
<spring:url value="/static/css/jquery.tagsinput.css" var="jqueryTagsInputCSS" />

<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title><spring:message code="generic.title" /></title>

<link href="${bootstrapCSS}" rel="stylesheet" media="all">
<link href="${bootstrapResponsiveCSS}" rel="stylesheet" media="all">
<link href="${datePickerCSS}" rel="stylesheet" media="all">
<link href="${dateRangePickerCSS}" rel="stylesheet" media="all">
<link href="${jqueryDataTablesCSS}" rel="stylesheet" media="all">
<link href="${dataTablesBootstrapCSS}" rel="stylesheet" media="all">
<link href="${awesomeFontCSS}" rel="stylesheet" media="all">
<link href="${catalogCSS}" rel="stylesheet" media="all">
<link href="${jqueryPinesNotifyCSS}" rel="stylesheet" media="all">
<link href="${jqueryTagsInputCSS}" rel="stylesheet" media="all">

<!--[if IE 7]>
	<link rel="stylesheet" href="${awesomeFontIE7CSS}">
	<![endif]-->

<%@include file="/WEB-INF/jsp/common/script.jsp"%>

<style type="text/css">
.map1 {
	height: 400px;
}

.mapcontent {
	background-color: white;
	padding: 44px 0px 0px 0px;
	-webkit-border-radius: 0 0 6px 6px;
	-moz-border-radius: 0 0 6px 6px;
	border-radius: 0 0 6px 6px;
	-webkit-box-shadow: 0 1px 2px rgba(0, 0, 0, .15);
	-moz-box-shadow: 0 1px 2px rgba(0, 0, 0, .15);
	box-shadow: 0 1px 2px rgba(0, 0, 0, .15);
}
/* bootstrap fix for google maps */
#map_canvas_1 img {
	max-width: none;
}
</style>

<c:if test="${alternative eq 'true'}">
	<spring:url value="/static/js/alternative.js" var="alternativeJS" />
	<script type="text/javascript" src="${alternativeJS}"></script>
	<script type="text/javascript">
		window.alternative = true;
		window.testPage = '${testPage}';
		window.contextPath = '${contextPath}';
	</script>
	<spring:url value="/static/css/alternative.css" var="alternativeCSS" />
	<link href="${alternativeCSS}" rel="stylesheet" media="all">
</c:if>

</head>
<body>

	<%@include file="/WEB-INF/jsp/common/navbar.jsp"%>