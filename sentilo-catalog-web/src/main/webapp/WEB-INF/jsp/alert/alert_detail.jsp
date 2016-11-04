<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', alert)"/>

<spring:url value="/admin/alert/${alert.id}/edit" var="editAlertLink" />
<spring:url value="/admin/alert/list?nameTableRecover=alertTable&fromBack=true" var="backURL" />

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
							${alert.name}<br /> <small><spring:message code="id" /> ${alert.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="alert.detail.title" /> </a></li>
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
															<strong><spring:message code="alert.tenantId" /> </strong>
														</div>
														<div class="span8">${alert.tenantId}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alert.description" /> </strong>
														</div>
														<div class="span8">${alert.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alert.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="alert.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alert.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="alert.updatedAt" />
														</div>
													</div>
													
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alert.active" /> </strong>
														</div>
														<div class="span8">
															<c:set var="alertActive" value="${fn:contains(alert.active, 'true') ? 'checked' : ''}"/>														
															<div class="span8"><input type="checkbox" ${alertActive} disabled="disabled" onclick="javascript:return false;"/></div>															
														</div>
													</div>
													
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alert.type" /> </strong>
														</div>
														<div class="span8">
															<span class="label label-info"><spring:message code="alert.type.${alert.type}" /> </span>
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alert.providerId" /> </strong>
														</div>
														<div class="span8">${alert.providerId}</div>
													</div>
													<c:if test="${alert.type eq 'INTERNAL'}">														
														<div class="row-fluid">
															<div class="span4">
																<strong><spring:message code="alert.componentId" /> </strong>
															</div>
															<div class="span8">${alert.componentId}</div>
														</div>
														<div class="row-fluid">
															<div class="span4">
																<strong><spring:message code="alert.sensorId" /> </strong>
															</div>
															<div class="span8">${alert.sensorId}</div>
														</div>
													</c:if>
													<c:if test="${alert.type eq 'EXTERNAL'}">
														<div class="row-fluid">
															<div class="span4">
																<strong><spring:message code="alert.applicationId" /> </strong>
															</div>
															<div class="span8">${alert.applicationId}</div>
														</div>
													</c:if>
												</div>
											</div>
										</div>
									</div>
									<c:if test="${alert.type eq 'INTERNAL'}">

										<div class="accordion" id="expressionAccordion">
											<div class="accordion-group">
												<div class="accordion-heading">
													<a class="accordion-toggle" data-toggle="collapse" data-parent="#expressionAccordion"
														href="#expressionAccordionCollapse"> <i class="icon-th"></i> <spring:message
															code="alert.expression.title" /> <i class="icon-chevron-down pull-right"></i> </a>
												</div>
												<div id="expressionAccordionCollapse" class="accordion-body collapse in">
													<div class="accordion-inner">
														<div class="row-fluid">
															<div class="span4">
																<strong><spring:message code="alert.trigger" /> </strong>
															</div>
															<div class="span8">
																<spring:message code="alert.trigger.${alert.trigger}" />
															</div>
														</div>
														<div class="row-fluid">
															<div class="span4">
																<strong><spring:message code="alert.expression" /> </strong>
															</div>
															<div class="span8">${alert.expression}</div>
														</div>
													</div>
												</div>
											</div>
										</div>
									</c:if>
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<c:if test="${showAdminControls}">
												<a href="${editAlertLink}" class="btn btn-primary"> 
													<spring:message code="alert.edit.title" /> 
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