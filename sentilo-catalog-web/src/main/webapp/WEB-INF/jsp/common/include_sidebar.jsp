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

<c:set value=" connecta-icon-black" var="classApplicationIcon" />
<c:set value=" connecta-icon-black" var="classProviderIcon" />
<c:set value=" connecta-icon-black" var="classSensorIcon" />
<c:set value=" connecta-icon-black" var="classAlertIcon" />
<c:set value=" connecta-icon-black" var="classUserIcon" />
<c:set value=" connecta-icon-black" var="classSensorTypeIcon" />
<c:set value=" connecta-icon-black" var="classComponentTypeIcon" />
<c:set value=" connecta-icon-black" var="classComponentIcon" />

<c:if test="${activeMenu == '/application' }">
	<c:set value=" class='current'" var="classApplication" />
	<c:set value=" icon-white" var="classApplicationIcon" />
</c:if>
<c:if test="${activeMenu == '/provider' }">
	<c:set value=" class='current'" var="classProvider" />
	<c:set value=" icon-white" var="classProviderIcon" />
</c:if>
<c:if test="${activeMenu == '/sensor' }">
	<c:set value=" class='current'" var="classSensor" />
	<c:set value=" icon-white" var="classSensorIcon" />
</c:if>
<c:if test="${activeMenu == '/alert' }">
	<c:set value=" class='current'" var="classAlert" />
	<c:set value=" icon-white" var="classAlertIcon" />
</c:if>
<c:if test="${activeMenu == '/user' }">
	<c:set value=" class='current'" var="classUser" />
	<c:set value=" icon-white" var="classUserIcon" />
</c:if>
<c:if test="${activeMenu == '/sensorType' }">
	<c:set value=" class='current'" var="classSensorType" />
	<c:set value=" icon-white" var="classSensorTypeIcon" />
</c:if>
<c:if test="${activeMenu == '/componentType' }">
	<c:set value=" class='current'" var="classComponentType" />
	<c:set value=" icon-white" var="classComponentTypeIcon" />
</c:if>
<c:if test="${activeMenu == '/component' || activeMenu == '/componentMap' }">
	<c:set value=" class='current'" var="classComponent" />
	<c:set value=" icon-white" var="classComponentIcon" />
</c:if>

<div id="menu-left" class="sidebar-nav">
	<ul class="nav nav-list">
		<security:authorize access="hasRole('ROLE_ADMIN')">
			<li class="nav-header"><spring:message code="sidebar.admin.title" />
			</li>
			<li><a href="${applicationListURL}"${classApplication}> <i class="icon-cog${classApplicationIcon}"></i> <spring:message
						code="menu.admin.application.title" /> </a>
			</li>
			<li><a href="${providerListURL}"${classProvider}> <i class="icon-exchange${classProviderIcon}"></i> <spring:message
						code="menu.admin.provider.title" /> </a>
			</li>
			<li><a href="${componentListURL}"${classComponent}> <i class="icon-cogs${classComponentIcon}"></i> <spring:message
						code="sidebar.component.title" /> </a>
			</li>
			<li><a href="${sensorListURL}"${classSensor}> <i class="icon-beaker${classSensorIcon}"></i> <spring:message
						code="sidebar.sensor.title" /> </a>
			</li>
			<li>
				<a href="${alertListURL}"${classAlert}> 
					<i class="icon-bell-alt${classAlertIcon}"></i><spring:message code="sidebar.alert.title" /> 
				</a>
			</li>
			<li><a href="${userListURL}"${classUser}> <i class="icon-user${classUserIcon}"></i> <spring:message
						code="menu.admin.user.title" /> </a>
			</li>
			<li><a href="${sensorTypesListURL}"${classSensorType}> <i class="icon-wrench${classSensorTypeIcon}"></i> <spring:message
						code="menu.admin.sensortype.title" /> </a>
			</li>
			<li><a href="${componentTypesListURL}"${classComponentType}> <i class="icon-adjust${classComponentTypeIcon}"></i>
					<spring:message code="menu.admin.componenttype.title" /> </a>
			</li>
		</security:authorize>
	</ul>
</div>

