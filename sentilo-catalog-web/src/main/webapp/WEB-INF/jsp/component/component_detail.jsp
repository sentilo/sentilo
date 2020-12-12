<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', component)"/>

<c:set var="componentTable"  value="componentDetailComponentTable" />
<c:set var="sensorTable"  value="componentDetailSensorTable" />
<c:set var="componentId" scope="request" value="${component.id}" />

<spring:url value="/admin/component/new" var="newComponentLink" />
<spring:url value="/admin/component/delete" var="deleteURL" />
<spring:url value="/admin/component/${component.id}/edit" var="editComponentLink" />
<spring:url value="/admin/sensor/list/json?componentId=${component.id}" var="sensorsAjaxSource" />
<spring:url value="/admin/component/${component.id}/removeSensors" var="deleteSensorsURL" />
<spring:url value="/admin/component/list?nameTableRecover=componentTable&fromBack=true" var="backURL" />
<spring:url value="/admin/component/list/json?parentId=${componentId}" var="sAjaxSourceComp" />
<spring:url value="/admin/sensor/list/excel?tableName=${sensorTable}&componentId=${component.id}" var="sensorExcelSource" />


<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp" %>

<spring:message code="sure.delete.component" var="deleteComponentConfirmMessage" />

<c:if test="${component.staticComponent}">
	<script type="text/javascript">	
	$(document).ready(function() {
		const componentPath = '${component.location}';
		const mapOptions = {
			latitude: '${component.location.centroid[1]}',
			longitude: '${component.location.centroid[0]}',
			coordinates: componentPath.split(','),
			icon: '${componentIcon}',
			provider: '${provider_map}'
		}
		initializeMap(mapOptions);
	});
	</script>
</c:if>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span12">
				<div class="span3">
					<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
				</div>
				<div class="span9">

					<div class="row-fluid">
						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							${component.name}<br /> <small><spring:message code="id" /> ${component.id}</small>
						</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="${tab1Class}"><a href="#tab1" data-toggle="tab"><spring:message code="component.detail.title" /></a></li>
								<li class="${tab2Class}"><a href="#tab2" data-toggle="tab"><spring:message code="technicalDetails.tab.label" /></a></li>
								<li class="${tab3Class}"><a href="#tab3" data-toggle="tab"><spring:message code="component.additionalInfo" /></a></li>
								<li class="${tab4Class}"><a href="#tab4" data-toggle="tab"><spring:message code="component.components" /></a></li>
								<li class="${tab5Class}"><a href="#tab5" data-toggle="tab"><spring:message code="sensor.list.title" /></a></li>
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
															<strong><spring:message code="component.tenantId" /> </strong>
														</div>
														<div class="span8">${component.tenantId}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.type" /> </strong>
														</div>
														<div class="span8">
															<span class="label label-info">${component.componentType}</span>
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.description" /> </strong>
														</div>
														<div class="span8">${component.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.providerId" /> </strong>
														</div>
														<div class="span8">${component.providerId}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.accessType" /> </strong>
														</div>
														<div class="span8">
															<c:choose>
																<c:when test="${component.publicAccess}">
																	<spring:message code="public" />
																</c:when>
																<c:otherwise>
																	<spring:message code="private" />
																</c:otherwise>
															</c:choose>
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="component.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="component.updatedAt" />
														</div>
													</div>
													<c:set value="${component.tagsAsList}" var="tags" scope="request" />
													<%@include file="/WEB-INF/jsp/common/include_detail_tags.jsp"%>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.location" /> </strong>
														</div>
														<div class="span8">
															<span class="label label-info"> 
																<c:choose>	
																	<c:when test="${component.mobileComponent}">
																		<spring:message code="mobile" />
																	</c:when> 
																	<c:otherwise>
																		<spring:message code="static" />
																	</c:otherwise>
																</c:choose>
															</span>
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
												<c:if test="${showAdminControls and not component.federatedResource}">
												<a href="${editComponentLink }" class="btn btn-primary"> 
													<spring:message code="component.edit.title" />
												</a>
												</c:if>
											</div>
										</div>
									</div>
									<c:if test="${component.staticComponent}">
										<br />
										<div class="row-fluid">
											<div class="span12">
												<%@include file="/WEB-INF/jsp/common/include_location_map.jsp"%>
											</div>
										</div>
									</c:if>
								</div>
								<div class="${tab2PaneClass}" id="tab2">
									<div class="accordion" id="technicalDetailsAccordion">
										<c:set var="technicalDetails"  value="${component.technicalDetails}" />
										<c:set var="resourceIsComponent"  value="true" />
										<%@include file="/WEB-INF/jsp/common/include_technical_details.jsp"%>
										<br/>
										<div class="row-fluid">
											<div class="span12">
												<div class="control-group pull-right">
													<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
													<c:if test="${showAdminControls and not component.federatedResource}">
													<a href="${editComponentLink }" class="btn btn-primary">
														<spring:message code="component.edit.title" />
													</a>
													</c:if>
												</div>
											</div>
										</div>
									</div>
								</div>
								<div class="${tab3PaneClass}" id="tab3">
									<div class="accordion" id="detailAdditionalInfoAccordion">
										<c:set var="additionalInfo"  value="${component.additionalInfo}" />
										<%@include file="/WEB-INF/jsp/common/include_additional_info.jsp"%>
										<br/>
										<div class="row-fluid">
											<div class="span12">
												<div class="control-group pull-right">
													<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
													<c:if test="${showAdminControls and not component.federatedResource}">
													<a href="${editComponentLink }" class="btn btn-primary">
														<spring:message code="component.edit.title" />
													</a>
													</c:if>
												</div>
											</div>
										</div>
									</div>
								</div>
								<div class="${tab4PaneClass}" id="tab4">
									<%@include file="/WEB-INF/jsp/common/include_component_list.jsp"%>
									<div class="control-group pull-right">
										<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
										<c:if test="${showAdminControls}">
										<spring:url value="/admin/component/${component.id}/addComponents" var="addComponentsURL" />
										<a href="#" onclick="deleteSelected('components');" class="btn btn-danger">
											<spring:message	code="component.unassign.title" />
										</a>
										<a href="#" onclick="window.location.href='${addComponentsURL}';" class="btn">
											<spring:message code="component.assign.title" />
										</a>
										</c:if>
									</div>
								</div>
								<div class="${tab5PaneClass}" id="tab5">
									<%@include file="/WEB-INF/jsp/common/include_sensor_list.jsp"%>									
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
