<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<spring:eval var="userTenantId" expression="T(org.sentilo.web.catalog.utils.TenantUtils).getMenuCurrentTenant()"/>
<spring:eval var="isFederationEnabled" expression="T(org.sentilo.web.catalog.security.SecurityUtils).isFederationEnabled()"/>
<spring:eval var="isMultitenant" expression="T(org.sentilo.web.catalog.context.TenantContextHolder).isEnabled()"/>

<security:authorize access="isAuthenticated()">
	<security:authentication var="principal" property="principal"/>
	<c:set var="isSectorial" value="${principal.sectorialUser}" />
	
	<spring:url value="/admin/provider/list?nameTableRecover=providerTable&uar=1&sfamr=true" var="providerListURL" />
	<spring:url value="/admin/application/list?nameTableRecover=applicationTable&uar=1&sfamr=true" var="applicationListURL" />
	<spring:url value="/admin/alert/list?nameTableRecover=alertTable&uar=1&sfamr=true" var="alertListURL" />
	<spring:url value="/admin/alertRule/list?nameTableRecover=alerRuleTable&uar=1&sfamr=true" var="alertRuleListURL" />
	<spring:url value="/admin/sensor/list?nameTableRecover=sensorTable&uar=1&sfamr=true" var="sensorListURL" />
	<spring:url value="/admin/users/list?nameTableRecover=userTable&uar=1&sfamr=true" var="userListURL" />
	<spring:url value="/admin/users/${principal.username}/detail" var="userDetailURL"/>
	<spring:url value="/admin/sensortypes/list?nameTableRecover=sensorTypeTable&uar=1&sfamr=true" var="sensorTypesListURL" />
	<spring:url value="/admin/componenttypes/list?nameTableRecover=componentTypeTable&uar=1&sfamr=true" var="componentTypesListURL" />
	<spring:url value="/admin/component/list?nameTableRecover=componentTable&uar=1&sfamr=true" var="componentListURL" />
	<security:authorize access="hasRole('ROLE_ADMIN')">
		<spring:url value="/admin/tenant/${userTenantId}/detail?uar=1&sfamr=true" var="tenantDetailURL" />
	</security:authorize>
	<spring:url value="/admin/tenant/list?nameTableRecover=tenantTable&uar=1&sfamr=true" var="tenantListURL" />
	<spring:url value="/admin/activesubscriptions/list?nameTableRecover=activesubscriptionsTable" var="activesubscriptionsListURL" />
	<spring:url value="/admin/federation/list?nameTableRecover=federationTable" var="federationListURL" />
	<spring:url value="/admin/metrics?sfamr=true" var="metricsURL" />
	<spring:url value="/admin/sector/list?nameTableRecover=sectorTable&uar=1&sfamr=true" var="sectorListURL" />
</security:authorize>	
 
<spring:url value="/j_spring_security_logout" var="logoutURL" />
