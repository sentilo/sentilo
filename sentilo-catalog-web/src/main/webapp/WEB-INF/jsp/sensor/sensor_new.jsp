<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="sensor.ttl.tooltip" var="ttlTooltip"/>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/sensor/${sensor.id}/edit" var="actionURL" />
	<spring:message code="sensor.edit.title" var="pageTitle" />
	<spring:url value="/admin/sensor/${sensor.id}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', sensor)"/>
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/component/search/json" var="componentSearchURL" />
	<spring:url value="/admin/sensor/create" var="actionURL" />
	<spring:message code="sensor.new.title" var="pageTitle" />
	<spring:message code="select.empty" var="emptySelectMessage" />
	<spring:url value="/admin/sensor/list?nameTableRecover=sensorTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE', 'org.sentilo.web.catalog.domain.Sensor')"/>
</c:if>

<c:if test="${not empty providerId}">
	<spring:url value="/admin/provider/${providerId}/detail" var="backURL" />
</c:if>

<c:set var="editMode" value="${mode == 'edit' }" />
<spring:message code="select.empty" var="emptySelectMessage" javaScriptEscape="false" htmlEscape="false"/>

<%@include file="/WEB-INF/jsp/common/include_script_datepickers.jsp"%>
<script type="text/javascript">
$(document).ready(function() {
	makeDateRangePicker('#validTime');
	makeDatePicker('#installationDateDatePicker');
	<c:if test="${not empty providerId && not editMode}">
		populateComponents();
	</c:if>	
	
});

function populateComponents() {
	var providerId = $('#providerId').val();
	var selectComponentId = $('#componentId');	
	if (providerId) {
		emptyComponentSelect();
		jsonGET('${componentSearchURL}?providerId=' + providerId, [], function(data) {
			for(var i=0; i<data.length; i++) {
				addComponentToSelect(selectComponentId, data[i]);
			}			
		});
	} 
};

function addComponentToSelect(select, component) {	
	select.append('<option value="' + component.id + '">' + component.name +'</option>');		
};

function emptyComponentSelect() {
	$('#componentId')
	    .find('option')
	    .remove()
	    .end()
	    .append('<option value="">${emptySelectMessage}</option>')
	    .val('');
};


function controlDisplaySubstateFields(){	
	$('#sensorSubstate').prop('selectedIndex', 0);
	
}
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

						<form:form method="post" modelAttribute="sensor" action="${actionURL}" class="form-horizontal" autocomplete="off">
						
							<spring:hasBindErrors name="sensor">
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
						
							<input type="hidden" id="tenantId" name="tenantId" value="${tenantId}" />
							<fieldset>
								<div class="tabbable">
									<ul class="nav nav-tabs">
										<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="sensor.detail.title" /></a></li>
										<li><a href="#tab2" data-toggle="tab"><spring:message code="technicalDetails.tab.label" /></a></li>
										<li><a href="#tab3" data-toggle="tab"><spring:message code="sensor.visualConfiguration" /></a></li>
										<li><a href="#tab4" data-toggle="tab"><spring:message code="sensor.additionalInfo" /></a></li>
									</ul>
									<c:if test="${editMode}">
										<form:hidden path="id" />
										<form:hidden path="createdAt" />
										<form:hidden path="createdBy" />
									</c:if>
									<div class="tab-content">
										<div class="tab-pane active" id="tab1">
											<div class="control-group">
												<form:label path="sensorId" class="control-label">
													<spring:message code="sensor.sensorId" />
												</form:label>

												<div class="controls">
													<form:input path="sensorId" readonly="${editMode}" />
													<form:errors path="sensorId" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
												<div class="control-group">
													<form:label path="providerId" class="control-label">
														<spring:message code="sensor.providerId" />
													</form:label>
													<div class="controls">
														<form:select path="providerId" id="providerId" onchange="populateComponents();" disabled="${editMode}">
															<form:option value="">${emptySelectMessage}</form:option>
															<form:options items="${providers}" itemValue="value" itemLabel="label" />
														</form:select>
														<c:if test="${editMode}">
															<form:hidden path="providerId" />
														</c:if>
														<form:errors path="providerId" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>
												<input type="hidden" name="origin" value="sensor" />

											<div class="control-group">
												<form:label path="description" class="control-label">
													<spring:message code="sensor.description" />
												</form:label>
												<div class="controls">
													<form:textarea path="description" />
													<form:errors path="description" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
											<div class="control-group">
												<form:label path="componentId" class="control-label">
													<spring:message code="sensor.componentId" />
												</form:label>
												<div class="controls">
													<form:select path="componentId" cssClass="input-large" disabled="${editMode}">
														<form:option value="">${emptySelectMessage}</form:option>
														<c:if test="${editMode}">
															<form:options items="${components}" itemValue="value" itemLabel="label" />
														</c:if>
													</form:select>
													<c:if test="${editMode}">
														<form:hidden path="componentId" />
													</c:if>
													<form:errors path="componentId" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>

											<div class="control-group">
												<form:label path="publicAccess" class="control-label">
													<spring:message code="sensor.publicAccess" />
												</form:label>
												<div class="controls">
													<form:checkbox path="publicAccess" />
													<form:errors path="publicAccess" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
											<div class="control-group">
												<form:label path="type" class="control-label">
													<spring:message code="sensor.type" />
												</form:label>
												<div class="controls">
													<form:select path="type">
														<form:options items="${sensorTypes}" itemValue="value" itemLabel="label" />
													</form:select>
													<form:errors path="type" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
											<div class="control-group">
												<form:label path="dataType" class="control-label">
													<spring:message code="sensor.dataType" />
												</form:label>
												<div class="controls">
													<form:select path="dataType">
														<form:options items="${sensorDataTypes}" itemValue="value" itemLabel="label" />
													</form:select>
													<form:errors path="dataType" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
											<div class="control-group">
												<form:label path="unit" class="control-label">
													<spring:message code="sensor.unit" />
												</form:label>
												<div class="controls">
													<form:input path="unit" />
													<form:errors path="unit" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
											<div class="control-group">
												<form:label path="timeZone" class="control-label">
													<spring:message code="sensor.timeZone" />
												</form:label>
												<div class="controls">
													<form:input path="timeZone" />
													<form:errors path="timeZone" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
											<div class="control-group">
												<form:label path="state" class="control-label">
													<spring:message code="sensor.state" />
												</form:label>
												<div class="controls">
													<form:select path="state" id="sensorState" onchange="controlDisplaySubstateFields();">
														<form:options items="${sensorStates}" itemValue="value" itemLabel="label" />
													</form:select>
													<form:errors path="state" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>
											<div class="control-group" id="substateFields">
												<form:label path="substate" class="control-label">
													<spring:message code="sensor.substate" />
												</form:label>
												<div class="controls">
													<form:select path="substate" id="sensorSubstate">														
														<form:option value="">${emptySelectMessage}</form:option>
														<form:options items="${sensorSubstates}" itemValue="code" itemLabel="description" />														
													</form:select>
													<form:errors path="substate" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>		
											<div class="control-group">
												<form:label path="ttl" id="ttl" class="control-label">
													<spring:message code="sensor.ttl"/>
												</form:label>
												<div class="controls">
													<form:input path="ttl" tooltip="${ttlTooltip}" />
													<form:errors path="ttl" cssClass="text-error" htmlEscape="false" />
												</div>
											</div>																				
											
											<%@include file="/WEB-INF/jsp/common/include_input_tags.jsp"%>
										</div>
										<div class="tab-pane" id="tab2">											
											<c:set var="resourceIsComponent"  value="false" />
											<%@include file="/WEB-INF/jsp/common/include_technical_details_new.jsp"%>
										</div>
										<div class="tab-pane" id="tab3">											
											<c:set var="resourceIsComponent" value="false" />
											<c:set var="isSensorConfiguration" value="true" />
											<%@include file="/WEB-INF/jsp/common/include_visual_configuration_new.jsp"%>
										</div>
										<div class="tab-pane" id="tab4">
											<c:set var="additionalInfo"  value="${sensor.additionalInfo}" />
											<%@include file="/WEB-INF/jsp/common/include_additional_info.jsp"%>
										</div>
									</div>
								</div>
								<br />
								<div class="control-group">
									<div class="controls pull-right">
										<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
										<c:if test="${showAdminControls}">
										<a href="#" onclick="$('form#sensor').submit();" class="btn btn-success"> <spring:message code="button.save" /> </a>
										</c:if>
									</div>
								</div>
							</fieldset>
						</form:form>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>