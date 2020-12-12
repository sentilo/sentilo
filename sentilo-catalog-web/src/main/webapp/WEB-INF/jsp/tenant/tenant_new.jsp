<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/tenant/${tenant.id}/edit" var="actionURL" />
	<spring:message code="tenant.edit.title" var="pageTitle" />
	<spring:url value="/admin/tenant/${tenant.id}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', tenant)"/>	
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/tenant/create" var="actionURL" />
	<spring:message code="tenant.new.title" var="pageTitle" />
	<spring:url value="/admin/tenant/list?nameTableRecover=tenantTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE', 'org.sentilo.web.catalog.domain.Tenant')"/>
</c:if>

<c:set var="editMode" value="${mode == 'edit' }" />

<spring:message code="select.empty" var="emptySelectMessage" javaScriptEscape="false" htmlEscape="false"/>

<script>
$(document).ready(function() {
	$('.colorPicker').colorpicker({'format':'hex'});
});
</script>


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
							${pageTitle}<br />
						</h1>
						
						<form:form method="post" modelAttribute="tenant" action="${actionURL}" class="form-horizontal" autocomplete="off">
														
							<spring:hasBindErrors name="tenant">
								<div class="alert alert-block alert-error">
									<button type="button" class="close" data-dismiss="alert">&times;</button>
									<h5><spring:message code="error.check.form.errors" /></h5>
									<ul>
									<c:forEach var="error" items="${errors.allErrors}">
										<spring:message code="${error.field}" text="${error.field}" var="defaultErrorFieldName"/>
										<li class="text-error"><strong><spring:message code="${error.objectName}.${error.field}" text="${defaultErrorFieldName}"/>:</strong> <spring:message message="${error}" /></li>
									</c:forEach>
									</ul>
								</div>
							</spring:hasBindErrors>
							
							<div class="tabbable">
								<ul class="nav nav-tabs">
									<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="component.detail.title" /></a></li>
									<li><a href="#tab2" data-toggle="tab"><spring:message code="tenant.params.title" /></a></li>
								</ul>
								<div class="tab-content">
									<div class="tab-pane active" id="tab1">
										<div class="control-group">
											<form:label path="id" class="control-label">
												<spring:message code="tenant.id" />
											</form:label>
											<c:if test="${editMode}">
												<form:hidden path="createdAt" />
												<form:hidden path="createdBy" />												
												<form:hidden path="isDefault" />
											</c:if>
											<div class="controls">
												<form:input path="id" readonly="${editMode}" />
												<form:errors path="id" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="name" class="control-label">
												<spring:message code="tenant.name" />
											</form:label>
											<div class="controls">
												<form:input path="name" />
												<form:errors path="name" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="description" class="control-label">
												<spring:message code="tenant.description" />
											</form:label>
											<div class="controls">
												<form:textarea path="description" />
												<form:errors path="description" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="contactName" class="control-label">
												<spring:message code="tenant.contactName" />
											</form:label>
											<div class="controls">
												<form:input path="contactName" />
												<form:errors path="contactName" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="contactEmail" class="control-label">
												<spring:message code="tenant.contactEmail" />
											</form:label>
											<div class="controls">
												<form:input path="contactEmail" />
												<form:errors path="contactEmail" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="isPublic" class="control-label">
												<spring:message code="tenant.isPublic" />
											</form:label>
											<div class="controls">
												<form:checkbox path="isPublic" />
												<form:errors path="isPublic" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
									</div>
									<div class="tab-pane" id="tab2">										
										<%@include file="/WEB-INF/jsp/common/include_visual_configuration_new.jsp"%>
										<br />
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#mapParamsAccordion" href="#mapParamsAccordionCollapse"> 
													<i class="icon-th"></i> <spring:message code="tenant.configParams.mapConfiguration" /> 
													<i class="icon-chevron-down pull-right"></i> 
												</a>
											</div>
											<div id="mapParamsAccordionCollapse" class="accordion-body collapse in">		
												<div class="accordion-inner">
													<div class="control-group">
														<div class="control-group">
															<form:label path="mapParams.zoomLevel" class="control-label">
																<spring:message code="tenant.mapParams.zoomLevel" />
															</form:label>
															<div class="controls">
																<form:input path="mapParams.zoomLevel" />
																<form:errors path="mapParams.zoomLevel" cssClass="text-error" htmlEscape="false" />
															</div>
														</div>
														<div class="control-group">
															<form:label path="mapParams.center.latitude" class="control-label">
																<spring:message code="tenant.mapParams.center.latitude" />
															</form:label>
															<div class="controls">
																<form:input path="mapParams.center.latitude" />
																<form:errors path="mapParams.center.latitude" cssClass="text-error" htmlEscape="false" />
															</div>
														</div>
														<div class="control-group">
															<form:label path="mapParams.center.longitude" class="control-label">
																<spring:message code="tenant.mapParams.center.longitude" />
															</form:label>
															<div class="controls">
																<form:input path="mapParams.center.longitude" />
																<form:errors path="mapParams.center.longitude" cssClass="text-error" htmlEscape="false" />
															</div>
														</div>
														<div class="control-group colorPicker">
															<form:label path="mapParams.bgColor" class="control-label">
																<spring:message code="tenant.mapParams.bgColor" />
															</form:label>
															<div class="controls">
																<form:input path="mapParams.bgColor" class="form-control" /><span class="input-group-addon"><i></i></span>
																<form:errors path="mapParams.bgColor" cssClass="text-error" htmlEscape="false" />
															</div>
														</div>
													</div>
												</div>
											</div>
										</div>
									</div>
								</div>
							</div>
							
							<br />
							
							<div class="control-group pull-right">
								<div class="controls">
									<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%> 
									<c:if test="${showAdminControls}">
									<a href="#" onclick="$('form#tenant').submit();" class="btn btn-primary"> 
										<spring:message code="button.save" /> 
									</a>
									</c:if>
								</div>
							</div>
														
						</form:form>

					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>