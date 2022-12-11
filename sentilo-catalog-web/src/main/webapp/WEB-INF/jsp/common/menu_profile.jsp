<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@ include file="/WEB-INF/jsp/common/include_restricted_vars.jsp"%>

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
						<c:if test="${!isSectorial}">
							<li><a href="${tenantDetailURL}"><spring:message code="menu.admin.tenant.title" /> </a></li>
						</c:if>
					</security:authorize>		
					<security:authorize access="hasRole('ROLE_ADMIN')">
						<li><a href="${sectorListURL}"><spring:message code="menu.admin.sector.title" /> </a></li>
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
						<c:if test="${!isMultitenant && !isSectorial}">
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