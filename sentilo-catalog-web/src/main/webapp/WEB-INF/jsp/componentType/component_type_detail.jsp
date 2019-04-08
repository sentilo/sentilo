<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', componentType)"/>

<spring:url value="/admin/componenttypes/${componentType.id}/edit" var="editComponentTypeLink" />
<spring:url value="/static/img/icons" var="iconsPath" />
<spring:url value="/admin/componenttypes/list?nameTableRecover=componentTypeTable&fromBack=true" var="backURL" />

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
							${componentType.name}<br /> <small><spring:message code="id" /> ${componentType.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="componenttype.detail.title" />
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
															<strong><spring:message code="componenttype.name" /> </strong>
														</div>
														<div class="span8">${componentType.name}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="componenttype.description" /> </strong>
														</div>
														<div class="span8">${componentType.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="componenttype.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="componentType.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="componenttype.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="componentType.updatedAt" />
														</div>
													</div>
													<c:if test="${not empty componentType.icon}">
														<div class="row-fluid">
															<div class="span4">
																<strong><spring:message code="componenttype.icon" /> </strong>
															</div>
															<div class="span8 connecta-icon-group">
																<img src="${iconsPath}/${componentType.icon}.png">
															</div>
														</div>
													</c:if>
												</div>
											</div>
										</div>
									</div>
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<c:if test="${showAdminControls}">	
												<a href="${editComponentTypeLink}" class="btn btn-primary"> 
													<spring:message code="componenttype.edit.title" /> 
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