<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', sensorType)"/>

<spring:url value="/admin/sensortypes/${sensorType.id}/edit" var="editSensorTypeLink" />
<spring:url value="/static/img" var="iconPrefix" />
<c:set var="sensorTypeTableId"  value="sensorTypeDetailSensorTypeTable" />
<spring:url value="/admin/sensortypes/list?nameTableRecover=sensorTypeTable&fromBack=true" var="backURL" />

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
							${sensorType.name}<br /> <small><spring:message code="id" /> ${sensorType.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="sensortype.detail.title" />
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
															<strong><spring:message code="sensortype.name" /> </strong>
														</div>
														<div class="span8">${sensorType.name}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="sensortype.description" /> </strong>
														</div>
														<div class="span8">${sensorType.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="sensortype.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="sensorType.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="sensortype.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="sensorType.updatedAt" />
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
												<a href="${editSensorTypeLink}" class="btn btn-primary">
													<spring:message code="sensortype.edit.title" />
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