<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

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
	
<%@include file="/WEB-INF/jsp/common/customStyles.jsp"%>