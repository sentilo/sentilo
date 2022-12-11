<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/static/js/jquery.flot.js" var="jqueryFlotJS" />
<script type="text/javascript" src="${jqueryFlotJS}"></script>
<spring:url value="/static/js/jquery.flot.categories.js" var="jqueryFlotCategoriesJS" />
<script type="text/javascript" src="${jqueryFlotCategoriesJS}"></script>
<spring:url value="/static/js/jquery.flot.resize.js" var="jqueryFlotResizeJS" />
<script type="text/javascript" src="${jqueryFlotResizeJS}"></script>
<spring:url value="/static/js/sentilo/scripts_graphics.js" var="scriptsGraphicsJS" />
<script type="text/javascript" src="${scriptsGraphicsJS}"></script>