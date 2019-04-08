<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:set var="alternative" value="${param.alternative}" />
<c:set var="testPage" value="${param.testPage}" />
<c:set var="contextPath" value="${pageContext.request.contextPath}" />
<c:set var="requestUriTokens" value="${fn:split(requestScope['javax.servlet.forward.servlet_path'], '/')}" />
<c:set var="currentSchema" value="${fn:contains(header['X-Forwarded-Proto'], 'https') ? 'https' : 'http'}"/>

<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta http-equiv="X-UA-Compatible" content="IE=9" />
<meta http-equiv="Cache-Control" content="no-cache" >
<meta http-equiv="Pragma" content="no-cache">
<meta http-equiv="Expires" content="0" >
<meta name="referrer" content="strict-origin-when-cross-origin">
<meta name="_csrf" content="${_csrf.token}"/>
<meta name="_csrf_header" content="${_csrf.headerName}"/>


<title><spring:message code="generic.title" /></title>

<%@include file="/WEB-INF/jsp/common/styles.jsp"%>
<%@include file="/WEB-INF/jsp/common/scripts.jsp"%>

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

<c:choose>
	<c:when test="${not empty tenantCustomParams and not empty tenantCustomParams.tenantId}">
		<spring:url value="/static/tenant/${tenantCustomParams.tenantId}/img/favicon.ico" var="faviconUrl" />
	</c:when>
	<c:otherwise>
		<spring:url value="/static/images/favicon.ico" var="faviconUrl" />
	</c:otherwise>
</c:choose>
<link href="${faviconUrl}" rel="shortcut icon">

</head>

<body class="sntl-${fn:join(requestUriTokens, ' sntl-')}">

	<%@include file="/WEB-INF/jsp/common/navbar.jsp"%>