<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/" var="homeURL" />

<div class="navbar navbar-fixed-top">
	<div class="navbar-inner">
		<div class="container-fluid">
			<a data-target=".nav-collapse" data-toggle="collapse" class="btn btn-navbar collapsed"> <span class="icon-bar"></span>
				<span class="icon-bar"></span> <span class="icon-bar"></span> </a>
			<div class="main">
				<a href="${homeURL}">
					<div class="logotype"></div>
				</a>
			</div>
			<div class="brand"></div>

			<div class="nav-collapse collapse navBarMaenu">
				<%@include file="/WEB-INF/jsp/common/menu_about.jsp"%>
				<%@include file="/WEB-INF/jsp/common/menu_explore.jsp"%>
				<div class="pull-right">
					<security:authorize access="isAuthenticated()">
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