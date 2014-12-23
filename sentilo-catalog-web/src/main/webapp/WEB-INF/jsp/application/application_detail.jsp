<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/application/${application.id}/edit" var="editApplicationLink" />
<spring:url value="/admin/application/list?nameTableRecover=applicationTable&fromBack=true" var="backURL" />


<c:set var="providerTableId"  value="applicationDetailProviderTable" />


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
							${application.name}<br /> <small><spring:message code="id" /> ${application.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="${tab1Class}"><a href="#tab1" data-toggle="tab"><spring:message
											code="application.detail.title" /> </a></li>
								<li class="${tab2Class}"><a href="#tab2" data-toggle="tab"><spring:message
											code="application.permissions" /> </a></li>
								<li class="${tab3Class}"><a href="#tab3" data-toggle="tab"><spring:message
											code="application.active.subscriptions" /> </a></li>
							</ul>
							<div class="tab-content">
								<div class="${tab1PaneClass}" id="tab1">
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
															<strong><spring:message code="application.description" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="application.description" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="token" /> </strong>
														</div>
														<div class="span8">${application.token}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="application.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="application.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="application.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="application.updateAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="application.email" /> </strong>
														</div>
														<div class="span8">
															<a href="mailto:${application.email}">${application.email}</a>
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
												<a href="${editApplicationLink}" class="btn btn-primary"> <spring:message code="application.edit.title" />
												</a>
											</div>
										</div>
									</div>
								</div>
								<div class="${tab2PaneClass}" id="tab2">
									<c:set var="applicationId" scope="request" value="${application.id}" />
									<jsp:include page="/WEB-INF/jsp/common/include_list_permission.jsp" />
								</div>
								<div class="${tab3PaneClass}" id="tab3">
									<c:set var="applicationId" scope="request" value="${application.id}" />
									<jsp:include page="/WEB-INF/jsp/application/include_application_subscriptions.jsp" />
									<div class="control-group pull-right">
										<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
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