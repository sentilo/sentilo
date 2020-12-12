<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<sec:authentication var="principal" property="principal"/>

<spring:eval var="userTenantId" expression="T(org.sentilo.web.catalog.utils.TenantUtils).getMenuCurrentTenant()"/>
<spring:eval var="isFederationEnabled" expression="T(org.sentilo.web.catalog.security.SecurityUtils).isFederationEnabled()"/>
<spring:eval var="isMultitenant" expression="T(org.sentilo.web.catalog.context.TenantContextHolder).isEnabled()"/>

<spring:url value="/admin/provider/list?nameTableRecover=providerTable&uar=1&sfamr=true" var="providerListURL" />
<spring:url value="/admin/application/list?nameTableRecover=applicationTable&uar=1&sfamr=true" var="applicationListURL" />
<spring:url value="/admin/alert/list?nameTableRecover=alertTable&uar=1&sfamr=true" var="alertListURL" />
<spring:url value="/admin/alertRule/list?nameTableRecover=alerRuleTable&uar=1&sfamr=true" var="alertRuleListURL" />
<spring:url value="/admin/sensor/list?nameTableRecover=sensorTable&uar=1&sfamr=true" var="sensorListURL" />
<spring:url value="/admin/users/list?nameTableRecover=userTable&uar=1&sfamr=true" var="userListURL" />
<spring:url value="/admin/sensortypes/list?nameTableRecover=sensorTypeTable&uar=1&sfamr=true" var="sensorTypesListURL" />
<spring:url value="/admin/componenttypes/list?nameTableRecover=componentTypeTable&uar=1&sfamr=true" var="componentTypesListURL" />
<spring:url value="/admin/component/list?nameTableRecover=componentTable&uar=1&sfamr=true" var="componentListURL" />
<security:authorize access="hasRole('ROLE_ADMIN')">
<spring:url value="/admin/tenant/${userTenantId}/detail?uar=1&sfamr=true" var="tenantDetailURL" />
</security:authorize>
<spring:url value="/admin/users/${principal.username}/detail" var="userDetailURL"/>
<spring:url value="/admin/tenant/list?nameTableRecover=tenantTable&uar=1&sfamr=true" var="tenantListURL" />
<spring:url value="/admin/activesubscriptions/list?nameTableRecover=activesubscriptionsTable" var="activesubscriptionsListURL" />
<spring:url value="/admin/federation/list?nameTableRecover=federationTable" var="federationListURL" />
<spring:url value="/admin/metrics?sfamr=true" var="metricsURL" />
 
<spring:url value="/j_spring_security_logout" var="logoutURL" />

<ul class="nav">
	<li id="fat-menu" class="dropdown">
		<a id="drop3" class="dropdown-toggle" data-toggle="dropdown" role="button" href="#"> 
			<i class="icon-user"></i> <%=request.getUserPrincipal().getName()%> <b class="caret"></b>
		</a>
		<ul class="dropdown-menu pull-right" aria-labelledby="drop3" role="menu">
			<security:authorize access="hasAnyRole('ROLE_SUPER_ADMIN','ROLE_ADMIN','ROLE_USER')">			
					<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">
						<li><a href="${tenantListURL}"><spring:message code="menu.admin.tenants.title" /> </a></li>
					</security:authorize>
					<security:authorize access="hasRole('ROLE_ADMIN')">
						<li><a href="${tenantDetailURL}"><spring:message code="menu.admin.tenant.title" /> </a></li>
					</security:authorize>
					<security:authorize access="hasAnyRole('ROLE_SUPER_ADMIN','ROLE_ADMIN')">
						<li><a href="${userListURL}"><spring:message code="menu.admin.user.title" /> </a></li>						
					</security:authorize>
					<security:authorize access="hasAnyRole('ROLE_USER')">
						<li><a href="${userDetailURL}"><spring:message code="menu.normal.user.title"/></a> </li>
					</security:authorize>
					<security:authorize access="hasAnyRole('ROLE_ADMIN','ROLE_USER')">
						<li><a href="${applicationListURL}"><spring:message code="menu.admin.application.title" /> </a></li>
						<li><a href="${providerListURL}"><spring:message code="menu.admin.provider.title" /> </a></li>
						<li><a href="${componentListURL}"><spring:message code="menu.admin.component.title" /> </a></li>
						<li><a href="${sensorListURL}"><spring:message code="menu.admin.sensor.title" /> </a></li>
						<li><a href="${alertListURL}"><spring:message code="menu.admin.alert.title" /> </a></li>
						<li><a href="${alertRuleListURL}"><spring:message code="menu.admin.alertrule.title" /> </a></li>
						<li><a href="${activesubscriptionsListURL}"><spring:message code="menu.admin.activesubscription.title" /> </a></li>
					</security:authorize>
					<li><a href="${sensorTypesListURL}"><spring:message code="menu.admin.sensortype.title" /> </a></li>
					<li><a href="${componentTypesListURL}"><spring:message code="menu.admin.componenttype.title" /> </a></li>
					<security:authorize access="hasRole('ROLE_ADMIN')">
						<c:if test="${isFederationEnabled}"><li><a href="${federationListURL}"><spring:message code="menu.admin.federation.title" /> </a></li></c:if>
					</security:authorize>									
					<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">				
						<li>
							<a href="${metricsURL}">								
								<spring:message	code="menu.admin.metrics" /> 
							</a>
						</li>				
					</security:authorize>
					<security:authorize access="hasRole('ROLE_ADMIN')">			
						<c:if test="${!isMultitenant}">
							<li>
								<a href="${metricsURL}">									
									<spring:message	code="menu.admin.metrics" /> 
								</a>
							</li>
						</c:if>
					</security:authorize>					
				<li class="divider"></li>
			</security:authorize>
			<li>
                <a href="#" onclick="document.getElementById('logout-form').submit();"> <spring:message code="logout" /> </a>

                <form id="logout-form" action="<c:url value="/j_spring_security_logout"/>" method="post" hidden="true">
                    <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
                </form>
			</li>
		</ul>
	</li>
</ul>