<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:set var="componentId" scope="request" value="${component.id}" />

<spring:url value="/admin/component/${component.id}/edit" var="editComponentLink" />
<spring:url value="/admin/sensor/list/json?componentId=${component.id}" var="sensorsAjaxSource" />
<spring:url value="/admin/component/${component.id}/removeSensors" var="deleteSensorsURL" />

<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>
<%@include file="/WEB-INF/jsp/common/include_script_maps.jsp"%>

<c:if test="${component.staticComponent}">
	<script type="text/javascript">	
	$(document).ready(function() {
		initializeMap('${component.location.latitude}','${component.location.longitude}', '${component.name}','${componentIcon}');
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
								<li class="${tab1Class}"><a href="#tab1" data-toggle="tab"><spring:message
											code="component.detail.title" /> </a></li>
								<li class="${tab2Class}"><a href="#tab2" data-toggle="tab"><spring:message code="component.components" />
								</a></li>
								<li class="${tab3Class}"><a href="#tab3" data-toggle="tab"><spring:message code="sensor.list.title" />
								</a></li>
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
															<strong><spring:message code="component.type" /> </strong>
														</div>
														<div class="span8">
															<span class="label">${component.componentType}</span>
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
															<c:if test="${component.publicAccess}">
																<spring:message code="public" />
															</c:if>
															<c:if test="${not component.publicAccess}">
																<spring:message code="private" />
															</c:if>
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
															<spring:eval expression="component.updateAt" />
														</div>
													</div>
													<c:set value="${component.tagsAsList}" var="tags" scope="request" />
													<%@include file="/WEB-INF/jsp/common/include_detail_tags.jsp"%>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="component.location" /> </strong>
														</div>
														<div class="span8">
															<span class="label"> <c:if test="${component.mobileComponent}">
																	<spring:message code="mobile" />
																</c:if> <c:if test="${not component.mobileComponent}">
																	<spring:message code="static" />
																</c:if> </span>
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
												<a href="${editComponentLink }" class="btn btn-primary"> <spring:message code="component.edit.title" />
												</a>
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
									<%@include file="/WEB-INF/jsp/common/include_list_component.jsp"%>
								</div>
								<div class="${tab3PaneClass}" id="tab3">
									<%@include file="/WEB-INF/jsp/common/include_list_sensor.jsp"%>
									<%@include file="/WEB-INF/jsp/sensor/include_sensor_assign_controls.jsp"%>
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