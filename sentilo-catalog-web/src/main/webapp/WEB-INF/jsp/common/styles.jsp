<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="${currentSchema}://fonts.googleapis.com/css?family=Source+Sans+Pro:200,300,400,700" var="googleFonts" />
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
<spring:url value="/static/css/bootstrap-colorpicker.min.css" var="bootstrapColorPickerCSS" />
<spring:url value="/static/css/jquery.jsonPresenter.css" var="jsonPresenterCSS" />
<spring:url value="/static/css/lightbox.min.css" var="lightboxCSS" />
<spring:url value="/static/css/chartist-plugin-legend.css" var="chartistPluginLegendCSS" />
<spring:url value="/static/css/chartist-plugin-tooltip.css" var="chartistPluginTooltipCSS" />
<spring:url value="/static/css/chartist.css" var="chartistCSS" />
<spring:url value="/static/css/media_players.css" var="mediaPlayersCSS" />
<spring:url value="/static/css/sentilo18.css" var="sentilo18CSS" />
<spring:url value="/static/css/metrics.css" var="metricsCSS" />

<link href="${googleFonts}" rel="stylesheet" media="all">
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
<link href="${bootstrapColorPickerCSS}" rel="stylesheet" media="all">
<link href="${jsonPresenterCSS}" rel="stylesheet" media="all">
<link href="${lightboxCSS}" rel="stylesheet" media="all">
<link href="${chartistPluginLegendCSS}" rel="stylesheet" media="all">
<link href="${chartistPluginTooltipCSS}" rel="stylesheet" media="all">
<link href="${chartistCSS}" rel="stylesheet" media="all">
<link href="${mediaPlayersCSS}" rel="stylesheet" media="all">
<link href="${metricsCSS}" rel="stylesheet" media="all">


<c:if test="${not empty currentRequestMapping}">
	<c:choose>
		<c:when test="${fn:contains(currentRequestMapping, '/component/map')}">
			<spring:url value="/static/css/universal_map.css" var="universalMapCSS" />
			<link href="${universalMapCSS}" rel="stylesheet" media="all">
		</c:when>
	</c:choose>
</c:if>

<!--[if IE 7]>
<link rel="stylesheet" href="${awesomeFontIE7CSS}">
<![endif]-->

<%-- Sentilo 1.8 styles overrides all ones --%>
<link href="${sentilo18CSS}" rel="stylesheet" media="all">

<%@include file="/WEB-INF/jsp/common/customStyles.jsp"%>

