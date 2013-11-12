<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="taglibs.jsp"%>
<!DOCTYPE html>
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">

	<spring:url value="/static/css/bootstrap.css" var="bootstrapCSS"/>
	<spring:url value="/static/css/styles.css" var="stylesCSS"/>
	<spring:url value="/static/app/css/sentilo.css" var="connectaCSS"/>
	<spring:url value="/" var="baseURL"/>
	
	<link href="${bootstrapCSS}" rel="stylesheet" media="screen">
	<link href="${stylesCSS}" rel="stylesheet" media="screen">
	<link href="${connectaCSS}" rel="stylesheet" media="screen">
	
	<%@include file="script.jsp"%>

</head>
<body>

<%@include file="navbar.jsp"%>