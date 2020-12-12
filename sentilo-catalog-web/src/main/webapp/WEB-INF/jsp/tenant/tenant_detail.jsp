<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', tenant)"/>

<spring:url value="/admin/tenant/${tenant.id}/edit" var="editTenantLink" />

<security:authorize access="hasRole('ROLE_SUPER_ADMIN')">
	<spring:url value="/admin/tenant/list?nameTableRecover=tenantTable&fromBack=true" var="backURL" />
</security:authorize>

<security:authorize access="hasRole('ROLE_ADMIN')">
	<c:set value="#" var="backURL" />
</security:authorize>

<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">

				<div class="row-fluid">
					<div class="span12">

						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							${tenant.name}<br /> <small><spring:message code="id" /> ${tenant.id}</small>
						</h1>
						
						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="${tab1Class}">
									<a href="#tab1" data-toggle="tab">
										<spring:message code="tenant.detail.title" />
									</a>
								</li>
								<li class="${tab2Class}">
									<a href="#tab2" data-toggle="tab">
										<spring:message code="tenant.params.title" />
									</a>
								</li>
								<security:authorize access="hasRole('ROLE_ADMIN') and T(org.sentilo.web.catalog.context.TenantContextHolder).isEnabled()">
								<li class="${tab3Class}">
									<a href="#tab3" data-toggle="tab">
										<spring:message	code="tenant.permissions.to" /> 
									</a>
								</li>
								<li class="${tab4Class}">
									<a href="#tab4" data-toggle="tab">
										<spring:message	code="tenant.permissions.from" />
									</a>
								</li>
								</security:authorize>
							</ul>							
							<div class="tab-content">
								<div class="${tab1PaneClass}" id="tab1">
									<div class="accordion" id="detailAccordion">
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion" href="#detailAccordionCollapse"> 
													<i class="icon-th"></i> <spring:message code="data" /> <i class="icon-chevron-down pull-right"></i> 
												</a>
											</div>
											<div id="detailAccordionCollapse" class="accordion-body collapse in">
												<div class="accordion-inner">
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.description" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.description" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.contactName" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.contactName" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.contactEmail" /> </strong>
														</div>
														<div class="span8">
															<a href="mailto:${tenant.contactEmail}">${tenant.contactEmail}</a>
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.isPublic" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.isPublic" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.createdBy" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.createdBy" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.updatedAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.updatedBy" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.updatedBy" />
														</div>
													</div>
												</div>
											</div>
										</div>
									</div>
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<c:if test="${showAdminControls}">
												<a href="${editTenantLink}" class="btn btn-primary"> <spring:message code="tenant.edit.title" /></a>
												</c:if>
											</div>
										</div>
									</div>
								</div>
								<div class="${tab2PaneClass}" id="tab2">
									<div class="accordion" id="detailAccordion">
										<c:set var="visualConfiguration" value="${tenant.visualConfiguration}" />
										<%@include file="/WEB-INF/jsp/common/include_visual_configuration.jsp"%>
									</div>
									<br />
									<div class="accordion" id="detailAccordion">
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion" href="#detailAccordionCollapse"> 
													<i class="icon-th"></i> <spring:message code="tenant.configParams.mapConfiguration" /> <i class="icon-chevron-down pull-right"></i> 
												</a>
											</div>
											<div id="detailAccordionCollapse" class="accordion-body collapse in">
												<div class="accordion-inner">
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.mapParams.zoomLevel" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.mapParams.zoomLevel" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.mapParams.center.latitude" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.mapParams.center.latitude" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.mapParams.center.longitude" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.mapParams.center.longitude" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="tenant.mapParams.bgColor" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="tenant.mapParams.bgColor" />
														</div>
													</div>
												</div>
											</div>
										</div>
									</div>
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<c:if test="${showAdminControls}">
												<a href="${editTenantLink}" class="btn btn-primary"> <spring:message code="tenant.edit.title" /></a>
												</c:if>
											</div>
										</div>
									</div>
								</div>
								<security:authorize access="hasRole('ROLE_ADMIN')">
								<div class="${tab3PaneClass}" id="tab3">
									<c:set var="tenantId" scope="request" value="${tenant.id}" />
									<jsp:include page="/WEB-INF/jsp/tenant/tenant_to_permissions.jsp" />
								</div>
								<div class="${tab4PaneClass}" id="tab4">
									<c:set var="tenantId" scope="request" value="${tenant.id}" />
									<jsp:include page="/WEB-INF/jsp/tenant/tenant_from_permissions.jsp" />
								</div>
								</security:authorize>
							</div>						    
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>