<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<sec:authentication var="principal" property="principal"/>
<spring:eval var="isFederationEnabled" expression="T(org.sentilo.web.catalog.security.SecurityUtils).isFederationEnabled()"/>
<spring:eval var="isMultitenant" expression="T(org.sentilo.web.catalog.context.TenantContextHolder).isEnabled()"/>

<spring:url value="/admin/provider/list?sfamr=true&nameTableRecover=providerTable" var="providerListURL" />
<spring:url value="/admin/application/list?sfamr=true&nameTableRecover=applicationTable" var="applicationListURL" />
<spring:url value="/admin/alert/list?sfamr=true&nameTableRecover=alertTable" var="alertListURL" />
<spring:url value="/admin/alertRule/list?sfamr=true&nameTableRecover=alertRuleTable" var="alertRuleListURL" />
<spring:url value="/admin/sensor/list?sfamr=true&nameTableRecover=sensorTable" var="sensorListURL" />
<spring:url value="/admin/users/list?sfamr=true&nameTableRecover=userTable" var="userListURL" />
<spring:url value="/admin/users/${principal.username}/detail" var="userDetailURL"/>
<spring:url value="/admin/sensortypes/list?sfamr=true&nameTableRecover=sensorTypeTable" var="sensorTypesListURL" />
<spring:url value="/admin/componenttypes/list?sfamr=true&nameTableRecover=componentTypeTable" var="componentTypesListURL" />
<spring:url value="/admin/component/list?sfamr=true&nameTableRecover=componentTable" var="componentListURL" />
<security:authorize access="hasRole('ROLE_ADMIN')">
	<spring:url value="/admin/tenant/${tenantCustomParams.tenantId}/detail?sfamr=true" var="tenantDetailURL" />
</security:authorize>
<spring:url value="/admin/tenant/list?sfamr=true&nameTableRecover=tenantTable" var="tenantListURL" />
<spring:url value="/admin/activesubscriptions/list?nameTableRecover=activeSubscriptionsTable" var="activeSubscriptionsListURL" />
<spring:url value="/admin/federation/list?nameTableRecover=federationTable" var="federationListURL" />
<spring:url value="/admin/metrics?sfamr=true" var="metricsURL" />

<c:set value=" connecta-icon-black" var="classApplicationIcon" />
<c:set value=" connecta-icon-black" var="classProviderIcon" />
<c:set value=" connecta-icon-black" var="classSensorIcon" />
<c:set value=" connecta-icon-black" var="classAlertIcon" />
<c:set value=" connecta-icon-black" var="classUserIcon" />
<c:set value=" connecta-icon-black" var="classSensorTypeIcon" />
<c:set value=" connecta-icon-black" var="classComponentTypeIcon" />
<c:set value=" connecta-icon-black" var="classComponentIcon" />
<c:set value=" connecta-icon-black" var="classRuleIcon" />
<c:set value=" connecta-icon-black" var="classTenantIcon" />
<c:set value=" connecta-icon-black" var="classActiveSubscriptionsIcon" />
<c:set value=" connecta-icon-black" var="classFederationIcon" />
<c:set value=" connecta-icon-black" var="classMetricsIcon" />
 
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
<c:if test="${activeMenu == '/alertRule' }">
	<c:set value=" class='current'" var="classRule" />
	<c:set value=" icon-white" var="classRuleIcon" />
</c:if>
<c:if test="${activeMenu == '/tenant'}">
	<c:set value=" class='current'" var="classTenant" />
	<c:set value=" icon-white" var="classTenantIcon" />
</c:if>
<c:if test="${activeMenu == '/activesubscriptions'}">
	<c:set value=" class='current'" var="classActiveSubscriptions" />
	<c:set value=" icon-white" var="classActiveSubscriptionsIcon" />
</c:if>
<c:if test="${activeMenu == '/federation'}">
	<c:set value=" class='current'" var="classFederation" />
	<c:set value=" icon-white" var="classFederationIcon" />
</c:if>

<c:if test="${activeMenu == '/metrics'}">
	<c:set value=" class='current'" var="classMetrics" />
	<c:set value=" icon-white" var="classMetricsIcon" />
</c:if>

<div id="menu-left" class="sidebar-nav">
	<ul class="nav nav-list">
		<security:authorize   access="hasAnyRole('ROLE_SUPER_ADMIN','ROLE_ADMIN','ROLE_USER')">
			<li class="nav-header">
				<spring:message code="sidebar.admin.title" />
			</li>
			<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">
			<li>
				<a href="${tenantListURL}"${classTenant}> 
					<i class="icon-briefcase${classTenantIcon}"></i>
					<spring:message code="sidebar.tenants.title" /> 
				</a>
			</li>
			</security:authorize>
			<security:authorize access="hasRole('ROLE_ADMIN')">
			<li>
				<a href="${tenantDetailURL}"${classTenant}> 
					<i class="icon-briefcase${classTenantIcon}"></i>
					<spring:message code="sidebar.tenant.title" />
				</a>
			</li>
			</security:authorize>
			<security:authorize access="hasAnyRole('ROLE_SUPER_ADMIN','ROLE_ADMIN')">
			<li>
				<a href="${userListURL}"${classUser}>
					<i class="icon-user${classUserIcon}"></i> 
					<spring:message	code="menu.admin.user.title" /> 
				</a>
			</li>
			</security:authorize>
			<security:authorize access="hasAnyRole('ROLE_USER')">
				<li>
					<a href="${userDetailURL}"${classUser}>
						<i class="icon-user${classUserIcon}"></i>
						<spring:message	code="menu.normal.user.title" />
					</a>
				</li>
			</security:authorize>
			<security:authorize access="hasAnyRole('ROLE_ADMIN','ROLE_USER')">
			<li>
				<a href="${applicationListURL}"${classApplication}>
					<i class="icon-cog${classApplicationIcon}"></i> 
					<spring:message code="menu.admin.application.title" /> 
				</a>
			</li>
			<li>
				<a href="${providerListURL}"${classProvider}>
					<i class="icon-exchange${classProviderIcon}"></i> 
					<spring:message	code="menu.admin.provider.title" /> 
				</a>
			</li>
			<li>
				<a href="${componentListURL}"${classComponent}>
					<i class="icon-cogs${classComponentIcon}"></i> 
					<spring:message	code="sidebar.component.title" /> 
				</a>
			</li>
			<li>
				<a href="${sensorListURL}"${classSensor}> 
					<i class="icon-beaker${classSensorIcon}"></i> 
					<spring:message	code="sidebar.sensor.title" /> 
				</a>
			</li>
			<li>
				<a href="${alertListURL}"${classAlert}>	
					<i class="icon-bell-alt${classAlertIcon}"></i>
					<spring:message code="sidebar.alert.title" /> 
				</a>
			</li>
			<li>
				<a href="${alertRuleListURL}"${classRule}> 
					<i class="icon-tasks${classRuleIcon}"></i>
					<spring:message code="menu.admin.alertrule.title" /> 
				</a>
			</li>
			<li>
				<a href="${activeSubscriptionsListURL}"${classActiveSubscriptions}>
					<i class="icon-check${classActiveSubscriptionsIcon}"></i> 
					<spring:message	code="menu.admin.activesubscription.title" /> 
				</a>
			</li>
			</security:authorize>
			<li>
				<a href="${sensorTypesListURL}"${classSensorType}> 
					<i class="icon-wrench${classSensorTypeIcon}"></i> 
					<spring:message	code="menu.admin.sensortype.title" /> 
				</a>
			</li>
			<li>
				<a href="${componentTypesListURL}"${classComponentType}> 
					<i class="icon-adjust${classComponentTypeIcon}"></i>
					<spring:message code="menu.admin.componenttype.title" />
				</a>
			</li>
			<security:authorize access="hasRole('ROLE_ADMIN')">
				<c:if test="${isFederationEnabled}">
					<li>
						<a href="${federationListURL}"${classFederation}> 
							<i class="icon-briefcase${classFederationIcon}"></i>
							<spring:message code="menu.admin.federation.title" />
						</a>
					</li>
				</c:if>	
			</security:authorize>			
			<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">				
					<li>
						<a href="${metricsURL}"${classMetrics}>
							<i class="icon-dashboard${classMetricsIcon}"></i> 
							<spring:message	code="menu.admin.metrics" /> 
						</a>
					</li>				
			</security:authorize>
			<security:authorize access="hasRole('ROLE_ADMIN')">			
				<c:if test="${!isMultitenant}">
					<li>
						<a href="${metricsURL}"${classMetrics}>
							<i class="icon-dashboard${classMetricsIcon}"></i> 
							<spring:message	code="menu.admin.metrics" /> 
						</a>
					</li>
				</c:if>
			</security:authorize>
		</security:authorize>
	</ul>
</div>

