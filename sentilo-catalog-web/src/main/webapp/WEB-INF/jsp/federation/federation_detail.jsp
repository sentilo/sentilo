<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', 'org.sentilo.web.catalog.domain.FederationConfig')"/>

<spring:url value="/admin/federation/${federationConfig.id}/edit" var="editFederationLink" />
<spring:url value="/admin/federation/list?nameTableRecover=federationTable&fromBack=true" var="backURL" />

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
							${federationConfig.name}<br /> <small><spring:message code="id" /> ${federationConfig.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="federation.detail.title" />
								</a></li>
							</ul>
							<div class="tab-content">
								<div class="tab-pane active" id="tab1">
									<div class="accordion" id="detailAccordion">
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion"
													href="#detailAccordionCollapse"> <i class="icon-th"></i> <spring:message code="data" /> <i
													class="icon-chevron-down pull-right"></i> </a>
											</div>
											<div id="detailAccordionCollapse" class="accordion-body collapse in">
												<div class="accordion-inner">
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="federation.name" /> </strong>
														</div>
														<div class="span8">${federationConfig.name}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="federation.description" /> </strong>
														</div>
														<div class="span8">${federationConfig.description}</div>
													</div>													
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="federation.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="federationConfig.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="federation.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="federationConfig.updatedAt" />
														</div>
													</div>
													
													<fieldset>
														<legend>
															<spring:message code="federation.service.config.app" />
														</legend>
														<div class="control-group">
															<div class="span4">
																<strong><spring:message code="federation.app_client.name" /> </strong>
															</div>
															<div class="span8">${federationConfig.appClientName}</div>															
														</div>
														<div class="control-group">
															<div class="span4">
																<strong><spring:message code="federation.app_client.token" /> </strong>
															</div>
															<div class="span8">${federationConfig.appClientToken}</div>
														</div>
														<div class="control-group">
															<div class="span4">
																<strong><spring:message code="federation.service.endpoint" /> </strong>
															</div>
															<div class="span8">${federationConfig.sourceEndpoint}</div>																														
														</div>
														<div class="control-group">
															<div class="span4">
																<strong><spring:message code="federation.last_sync" /> </strong>
															</div>
															<div class="span8">
																<spring:eval expression="federationConfig.lastSyncTime" />
															</div>																														
														</div>
													</fieldset>	
													
													<fieldset>
														<legend>
															<spring:message code="federation.service.contact" />
														</legend>
														
														<div class="control-group">
															<div class="span4">
																<strong><spring:message code="federation.service.contact.name" /> </strong>
															</div>
															<div class="span8">${federationConfig.sourceContactName}</div>																														
														</div>
														<div class="control-group">
															<div class="span4">
																<strong><spring:message code="federation.service.contact.email" /> </strong>
															</div>
															<div class="span8">${federationConfig.sourceContactMail}</div>
														</div>
													</fieldset>																									
												</div>
											</div>
										</div>
									</div>
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<c:if test="${showAdminControls}">	
													<a href="${editFederationLink}" class="btn btn-primary"> 
														<spring:message code="federation.edit.title" /> 
													</a>
												</c:if>
											</div>
										</div>
									</div>
								</div>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>