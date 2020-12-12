<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:set var="providerId" scope="request" value="${provider.id}" />
<c:set var="sensorTable"  value="providerDetailSensorTable" />

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', provider)"/>
<spring:eval  var="showActiveSubscriptionsTab" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', provider)"/>
<spring:eval  var="showDocsTab" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', provider)"/>

<spring:url value="/admin/provider/${provider.id}/edit" var="editProviderLink" />
<spring:url value="/admin/sensor/list/json?providerId=${providerId}" var="sensorsAjaxSource" />
<spring:url value="/admin/sensor/list/excel?tableName=${sensorTable}&providerId=${providerId}" var="sensorExcelSource" />
<spring:url value="/admin/provider/list?nameTableRecover=providerTable&fromBack=true" var="backURL" />

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
							${provider.name}<br /> <small><spring:message code="id" /> ${provider.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="${tab1Class}"><a href="#tab1" data-toggle="tab"><spring:message code="provider.detail.title" />
								</a></li>
								<li class="${tab2Class}"><a href="#tab2" data-toggle="tab"><spring:message code="provider.sensors" />
								</a></li>
								<li class="${tab3Class}"><a href="#tab3" data-toggle="tab"><spring:message code="provider.components" />
								</a></li>
								<c:if test="${showActiveSubscriptionsTab}">
								<li class="${tab4Class}"><a href="#tab4" data-toggle="tab"><spring:message code="subscriptions.active.tab.label" />
								</a></li>
								</c:if>
								<c:if test="${showDocsTab}">
								<li class="${tab5Class}"><a href="#tab5" data-toggle="tab"><spring:message code="provider.docs" /> </a></li>
								</c:if>
							</ul>
							<div class="tab-content">
								<div class="${tab1PaneClass }" id="tab1">
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
															<strong><spring:message code="token" /> </strong>
														</div>
														<div class="span8">${provider.token}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="provider.description" /> </strong>
														</div>
														<div class="span8">${provider.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="provider.restHttps" /> </strong>
														</div>
														<c:set var="restHttpsChecked" value="${fn:contains(provider.restHttps, 'true') ? 'checked' : ''}"/>														
														<div class="span8"><input type="checkbox" ${restHttpsChecked} disabled="disabled" onclick="javascript:return false;"/></div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="entity.apiInputQuota" /> </strong>
														</div>
														<div class="span8">															
															<spring:eval expression="provider.apiInputQuota" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="entity.apiOutputQuota" /> </strong>
														</div>
														<div class="span8">															
															<spring:eval expression="provider.apiOutputQuota" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="provider.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="provider.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="provider.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="provider.updatedAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="provider.contact.name" /> </strong>
														</div>
														<div class="span8">${provider.contact.name}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="provider.contact.email" /> </strong>
														</div>
														<div class="span8">
															<a href="mailto:${provider.contact.email}">${provider.contact.email}</a>
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
												<c:if test="${showAdminControls and not provider.federatedResource}">
												<a href="${editProviderLink }" class="btn btn-primary"> <spring:message code="provider.edit.title" /> </a>
												</c:if>
											</div>
										</div>
									</div>
								</div>
								<div class="${tab2PaneClass}" id="tab2">
									<%@include file="/WEB-INF/jsp/common/include_sensor_list.jsp"%>
									<div class="control-group pull-right">
										<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
									</div>
								</div>
								<div class="${tab3PaneClass}" id="tab3">
									<jsp:include page="/WEB-INF/jsp/common/include_component_list.jsp" />
									<div class="control-group pull-right">
										<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
									</div>
								</div>
								<c:if test="${showActiveSubscriptionsTab}">
								<div class="${tab4PaneClass}" id="tab4">
									<jsp:include page="/WEB-INF/jsp/common/include_subscriptions.jsp" />
									<div class="control-group pull-right">
										<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
									</div>
								</div>
								</c:if>
								<c:if test="${showDocsTab}">
									<div class="${tab5PaneClass}" id="tab5">
										<jsp:include page="/WEB-INF/jsp/provider/provider_documents_list.jsp" />
									</div>
								</c:if>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>