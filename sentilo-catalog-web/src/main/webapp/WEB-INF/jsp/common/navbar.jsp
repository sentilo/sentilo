<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
<spring:url value="/static/img/logo_header.png" var="logoURL" />
<spring:url value="/" var="homeURL" />
<spring:message code="home.logo.alt" var="logoALT" />
<div class="navbar navbar-fixed-top">
	<div class="navbar-inner">
		<div class="container-fluid">
			<a data-target=".nav-collapse" data-toggle="collapse" class="btn btn-navbar collapsed"> <span class="icon-bar"></span>
				<span class="icon-bar"></span> <span class="icon-bar"></span> </a>
			<div class="main">
				<div class="logotype">
					<a href="${homeURL}"><img height="45" src="${logoURL}" alt="${logoALT}"> </a>
				</div>
			</div>
			<div class="brand"></div>

			<div class="nav-collapse collapse">
				<%@include file="/WEB-INF/jsp/common/menu_about.jsp"%>
				<%@include file="/WEB-INF/jsp/common/menu_explore.jsp"%>
				<div class="pull-right">
					<security:authorize access="isAuthenticated()">
						<%@include file="/WEB-INF/jsp/common/menu_search.jsp"%>
						<%@include file="/WEB-INF/jsp/common/menu_profile.jsp"%>
					</security:authorize>
					<security:authorize access="isAnonymous()">
						<%@include file="/WEB-INF/jsp/common/menu_signin.jsp"%>
					</security:authorize>
				</div>
			</div>
		</div>
	</div>
</div>