<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/provider/list?nameTableRecover=providerTable" var="providerListURL" />
<spring:url value="/admin/application/list?nameTableRecover=applicationTable" var="applicationListURL" />
<spring:url value="/admin/alert/list?nameTableRecover=alertTable" var="alertListURL" />
<spring:url value="/admin/sensor/list?nameTableRecover=sensorTable" var="sensorListURL" />
<spring:url value="/admin/users/list?nameTableRecover=userTable" var="userListURL" />
<spring:url value="/admin/sensortypes/list?nameTableRecover=sensorTypeTable" var="sensorTypesListURL" />
<spring:url value="/admin/componenttypes/list?nameTableRecover=componentTypeTable" var="componentTypesListURL" />
<spring:url value="/admin/component/list?nameTableRecover=componentTable" var="componentListURL" />

<spring:url value="/j_spring_security_logout" var="logoutURL" />

<ul class="nav">
	<li id="fat-menu" class="dropdown">
		<a id="drop3" class="dropdown-toggle" data-toggle="dropdown" role="button" href="#"> 
			<i class="icon-user"></i> <%=request.getUserPrincipal().getName()%> <b class="caret"></b> 
		</a>
		<ul class="dropdown-menu" aria-labelledby="drop3" role="menu">
			<security:authorize access="hasRole('ROLE_ADMIN')">
				<li class="dropdown-submenu pull-left">
					<a tabindex="-1" href="#"><spring:message code="menu.admin.title" /></a>
					<ul class="dropdown-menu">
						<li><a href="${applicationListURL}"><spring:message code="menu.admin.application.title" /> </a></li>
						<li><a href="${providerListURL}"><spring:message code="menu.admin.provider.title" /> </a></li>
						<li><a href="${componentListURL}"><spring:message code="menu.admin.component.title" /> </a></li>
						<li><a href="${sensorListURL}"><spring:message code="menu.admin.sensor.title" /> </a></li>
						<li><a href="${alertListURL}"><spring:message code="menu.admin.alert.title" /> </a></li>
						<li><a href="${userListURL}"><spring:message code="menu.admin.user.title" /> </a></li>
						<li><a href="${sensorTypesListURL}"><spring:message code="menu.admin.sensortype.title" /> </a></li>
						<li><a href="${componentTypesListURL}"><spring:message code="menu.admin.componenttype.title" /> </a></li>
					</ul>
				</li>
				<li class="divider"></li>
			</security:authorize>
				<li><a href="${logoutURL}"> <spring:message code="logout" /> </a></li>
		</ul>
	</li>
</ul>