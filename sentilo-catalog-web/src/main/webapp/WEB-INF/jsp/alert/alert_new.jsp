<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/alert/${alert.id}/edit" var="actionURL" />
	<spring:message code="alert.edit.title" var="pageTitle" />
	<spring:url value="/admin/alert/${alert.id}/detail" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('EDIT', alert)"/>
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/alert/create" var="actionURL" />
	<spring:message code="alert.new.title" var="pageTitle" />
	<spring:url value="/admin/alert/list?nameTableRecover=alertTable&fromBack=true" var="backURL" />
	<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE','org.sentilo.web.catalog.domain.Alert')"/>
</c:if>

<c:set var="editMode" value="${mode == 'edit' }" />

<spring:url value="/admin/sensor/search/json" var="sensorSearchURL" />
<spring:url value="/admin/component/search/json" var="componentSearchURL" />



<spring:message code="alert.sensorId.instructions" var="sensorIdPlaceholder" javaScriptEscape="false" htmlEscape="false" />
<spring:message code="alert.sensorId.instructions.noproviderSelected" var="sensorIdPlaceholderNoProviderSelected"
	javaScriptEscape="false" htmlEscape="false" />

<spring:message code="select.empty" var="emptySelectMessage" javaScriptEscape="false" htmlEscape="false"/>
<spring:message code="alert.error.provider.notselected" var="errorNoProviderSelected" javaScriptEscape="false" htmlEscape="false"/>

<script type="text/javascript">
function toSuggestionList(data) {
	var result = [];
	$.each(data, function(index, sensor) {
		result.push(sensor.sensorId);
	});
	return result;
}


function resetComponentSelect() {
	$('#componentId')
	    .find('option')
	    .remove()
	    .end()
	    .append('<option value="">${emptySelectMessage}</option>')
	    .val($("#componentId option:first").val());
}

function resetProviderSelect() {
	$("#providerId").val($("#providerId option:first").val());
	resetComponentSelect();
}

function resetClientAppSelect() {
	$("#applicationId").val($("#applicationId option:first").val());
}

function changeSensorIdInputBehaviour(text, disabled) {
	var sensorIdInput = $('#sensorId'); 
	sensorIdInput.attr('placeholder', text);
	if (disabled) {
		sensorIdInput.val('');
	}
	sensorIdInput.prop('disabled', disabled);
}

function enableSensorIdInput() {
	changeSensorIdInputBehaviour('${sensorIdPlaceholder}', false);
}

function disableSensorIdInput() {
	changeSensorIdInputBehaviour('${sensorIdPlaceholderNoProviderSelected}', true);
}

function addComponentToSelect(select, component) {
	<c:if test="${mode == 'create' }">
		select.append('<option value="' + component.id + '">' + component.name +'</option>');
	</c:if>
	<c:if test="${mode == 'edit' }">
		if (component.id === '${alert.componentId}') {
			select.append('<option value="' + component.id + '" selected>' + component.name +'</option>');
		} else {
			select.append('<option value="' + component.id + '">' + component.name +'</option>');
		}
	</c:if>
}

function populateComponents() {
	var providerId = $('#providerId').val();
	var selectComponentId = $('#componentId');
	resetComponentSelect();
	if (providerId) {
		jsonGET('${componentSearchURL}?providerId=' + providerId, [], function(data) {
			for(var i=0; i<data.length; i++) {
				addComponentToSelect(selectComponentId, data[i]);
			}
			if (selectComponentId.val()) {
				enableSensorIdInput();
			} else {
				disableSensorIdInput();
			}
		});
	} else {
		disableSensorIdInput();
	}
}

function populateSensorId() {
	var componentId = $('#componentId').val();
	if (componentId) { 
		enableSensorIdInput();
	} else {
		disableSensorIdInput();
	}
}

function initSensorIdTypeahead() {
	$('#sensorId').typeahead({
	    source: function (query, process) {
	    	
	    	var providerId = $("#providerId").val();
	    	var componentId = $("#componentId").val();
	    	
	    	if (!providerId || !componentId) {
		    	showErrorNotification('${errorNoProviderSelected}', '${errorNoProviderSelected}');
		    	return;
	    	}

	    	var url = '${sensorSearchURL}?search=' + query + "&providerId=" + providerId + '&componentId=' + componentId;;
	        return $.get(url, { }, function (data) {
	            return process(toSuggestionList(data));
	        });
	    }
	});
}

function togglealertType(newType) {
	if (newType === "EXTERNAL") {
		showExternalFields();
		disableSensorIdInput();
		resetProviderSelect();		
	} else {
		showInternalFields();
		resetProviderSelect();
		resetClientAppSelect();
	}
}

function activealertTypeFields() {
    var type = $('#alertType').val();

	if (type === "EXTERNAL") {
		showExternalFields();
		disableSensorIdInput();				
	} else {
		showInternalFields();		
	}
}

function showExternalFields(){
	$('#applicationIdFields').show();
	$('#providerIdFields').show();
	$('#componentIdFields').hide();
	$('#sensorIdFields').hide();
	$('#expressionFields').hide();
	$('#notificationFields').hide();
}

function showInternalFields(){
	$('#applicationIdFields').hide();
	$('#sensorIdFields').show();
	$('#componentIdFields').show();
	$('#providerIdFields').show();
	$('#expressionFields').show();
	$('#notificationFields').show();
}

$(document).ready(function() {
	activealertTypeFields();
	populateComponents();
	initSensorIdTypeahead();
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

						<form:form method="post" modelAttribute="alert" action="${actionURL}" class="form-horizontal" autocomplete="off">
							<form:errors cssClass="text-error" />
							<input type="hidden" id="tenantId" name="tenantId" value="${tenantId}" />
							<div class="control-group">
								<form:label path="id" class="control-label">
									<spring:message code="alert.id" />
								</form:label>
								<c:if test="${editMode}">
									<form:hidden path="createdAt" />
									<form:hidden path="createdBy" />
								</c:if>
								<div class="controls">
									<form:input path="id" readonly="${editMode}" />
									<form:errors path="id" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="name" class="control-label">
									<spring:message code="alert.name" />
								</form:label>
								<div class="controls">
									<form:input path="name" />
									<form:errors path="name" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="description" class="control-label">
									<spring:message code="alert.description" />
								</form:label>
								<div class="controls">
									<form:textarea path="description" />
									<form:errors path="description" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="active" class="control-label">
									<spring:message code="alert.active" />
								</form:label>
								<div class="controls">
									<form:checkbox path="active" />
									<form:errors path="active" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>							
							<div class="control-group">
								<form:label path="type" class="control-label">
									<spring:message code="alert.type" />
								</form:label>
								<div class="controls">
									<form:select path="type" onchange="togglealertType(this.value);" id="alertType" disabled="${editMode}">
										<form:options items="${alertTypes}" itemValue="value" itemLabel="label" />
									</form:select>
									<c:if test="${editMode}">
										<form:hidden path="type" />
									</c:if>
									<form:errors path="type" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>							
							<div class="control-group" id="providerIdFields">
								<form:label path="providerId" class="control-label">
									<spring:message code="alert.providerId" />
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
							<div class="control-group" id="componentIdFields">
								<form:label path="componentId" class="control-label">
									<spring:message code="alert.componentId" />
								</form:label>
								<div class="controls">
									<form:select path="componentId" id="componentId" onchange="populateSensorId();" disabled="${editMode}">
										<form:option value="">${emptySelectMessage}</form:option>
									</form:select>
									<c:if test="${editMode}">
										<form:hidden path="componentId" />
									</c:if>
									<form:errors path="componentId" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group" id="sensorIdFields">
								<form:label path="sensorId" class="control-label">
									<spring:message code="alert.sensorId" />
								</form:label>
								<div class="controls">
									<form:input path="sensorId" id="sensorId" placeholder="${sensorIdPlaceholder}" readonly="${editMode}" />
									<form:errors path="sensorId" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group" id="applicationIdFields">
								<form:label path="applicationId" class="control-label">
									<spring:message code="alert.applicationId" />
								</form:label>
								<div class="controls">
									<form:select path="applicationId" id="applicationId" disabled="${editMode}">
										<form:option value="">${emptySelectMessage}</form:option>
										<form:options items="${applications}" itemValue="value" itemLabel="label" />																				
									</form:select>
									<c:if test="${editMode}">
										<form:hidden path="applicationId" />
									</c:if>
									<form:errors path="applicationId" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>

							<div id="expressionFields">
								<h1 class="lead">
									<spring:message code="alert.expression.title" />
									<br />
								</h1>
								<div class="control-group">
									<form:label path="expression" class="control-label">
										<spring:message code="alert.trigger" />
									</form:label>
									<div class="controls">
										<form:select path="trigger" id="trigger">
											<form:options items="${alertTriggers}" itemValue="value" itemLabel="label" />
										</form:select>
										<form:errors path="trigger" cssClass="text-error" htmlEscape="false" />
									</div>
								</div>
								<div class="control-group">
									<form:label path="expression" class="control-label">
										<spring:message code="alert.expression" />
									</form:label>
									<div class="controls">
										<form:input path="expression" id="expression" />
										<form:errors path="expression" cssClass="text-error" htmlEscape="false" />
									</div>
								</div>
							</div>


							<div class="control-group">
								<div class="controls">
									<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
									<c:if test="${showAdminControls}"> 
									<a href="#" onclick="$('form#alert').submit();" class="btn btn-primary"> 
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