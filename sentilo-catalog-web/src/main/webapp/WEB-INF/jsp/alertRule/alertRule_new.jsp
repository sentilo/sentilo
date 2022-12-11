<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="alertrule.new.title" var="pageTitle" />
<spring:url value="/admin/alertRule/create" var="actionURL" />
<spring:url value="/admin/alertRule/list?nameTableRecover=alertRuleTable&fromBack=true" var="backURL" />
<spring:url value="/admin/componenttypes/search/json" var="componentTypesListURL" />
<spring:url value="/admin/sensortypes/search/json" var="sensorTypesListURL" />
<spring:url value="/admin/alertRule/validate" var="validationURL" />
<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('CREATE','org.sentilo.web.catalog.domain.AlertRule')"/>

<script type="text/javascript">
	
	function populateComponentTypes() {
		var providerId = $('#providerId').val();
		var selectComponentType = $('#componentType');
		resetComponentTypeSelect();
		
		// If providerId is null return all the types
		jsonGET('${componentTypesListURL}?providerId=' + providerId, [], function(data) {
			for(var i=0; i<data.length; i++) {
				addComponentTypeToSelect(selectComponentType, data[i]);
			}
		});
	}
	
	function resetComponentTypeSelect() {
		$('#componentType')
		    .find('option')
		    .remove()
		    .end()
		    .append('<option value="">${emptySelectMessage}</option>')
		    .val($("#componentType option:first").val());
	}
	
	function addComponentTypeToSelect(select, componentType) {
		select.append('<option value="' + componentType.id + '">' + componentType.name +'</option>');
	}
	
	function populateSensorTypes() {
		var providerId = $('#providerId').val();
		var selectSensorType = $('#sensorType');
		resetSensorTypeSelect();
		
		// If providerId is null return all the types
		jsonGET('${sensorTypesListURL}?providerId=' + providerId, [], function(data) {
			for(var i=0; i<data.length; i++) {
				addSensorTypeToSelect(selectSensorType, data[i]);
			}
		});
	}
	
	function resetSensorTypeSelect() {
		$('#sensorType')
		    .find('option')
		    .remove()
		    .end()
		    .append('<option value="">${emptySelectMessage}</option>')
		    .val($("#sensorType option:first").val());
	}
	
	function addSensorTypeToSelect(select, sensorType) {
		select.append('<option value="' + sensorType.id + '">' + sensorType.name +'</option>');
	}
	
	function collectFormData($fields) {
	    var data = {};
	    for (var i = 0; i < $fields.length; i++) {
	        var item = $($fields[i]);
	        data[item.attr("id")] = item.val();
	    }
	    return data;
	}

	function clearErrors($fields) {
	    for (var i = 0; i < $fields.length; i++) {
	        var item = $($fields[i]);
	        $("#"+item.attr("id")).parents(".control-group").removeClass("error");
	        $("#"+item.attr("id")).siblings(".help-inline").html("");
	        $("#"+item.attr("id")).parents(".controls").find(".text-error").remove();
	    }
	}

	function markErrors(errors) {
	    $.each(errors, function(key, val) {
	        $("#"+key).parents(".control-group").addClass("error");
	        $("#"+key).siblings(".help-inline").html(val);
	        $("#"+key).parents(".controls").append('<span id="'+key+'.errors" class="text-error">'+val+'</span>');
	    });
	}
	
	function showConfirmCreateRuleModalWindow() {
		validateFormAndShowConfirmModal($('#providerId').val(),$('#componentType option:selected').val(),$('#sensorType option:selected').val());
	}
	
	function validateFormAndShowConfirmModal(providerId, componentType, sensorType) {
	    var $form = $('form#alertRule');
	    var $fields = $form.find(":input");
	    clearErrors($fields);
	    
	    var data = collectFormData($fields);
	    var validationUrl = "${validationURL}";
	    
	    $.get(validationUrl, data, function(response) {
	        if (response.status == "FAIL") {
	        	markErrors(response.errors);
	        } else {
	        	clearErrors($fields);
	            showConfirmModal(providerId, componentType, sensorType);
	        }
	    }, "json");
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
						
						<form:form method="post" modelAttribute="alertRule" action="${actionURL}" class="form-horizontal" autocomplete="off">
							<form:errors cssClass="text-error" />
							<input type="hidden" id="tenantId" name="tenantId" value="${tenantId}" />
							<div class="control-group">
								<form:label path="name" class="control-label">
									<spring:message code="alertrule.name" />
								</form:label>
								<div class="controls">
									<form:input path="name" />
									<form:errors path="name" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="description" class="control-label">
									<spring:message code="alertrule.description" />
								</form:label>
								<div class="controls">
									<form:textarea path="description" />
									<form:errors path="description" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group" id="providerIdFields">
								<form:label path="providerId" class="control-label">
									<spring:message code="alertrule.providerId" />
								</form:label>
								<div class="controls">
									<form:select path="providerId" id="providerId" onchange="populateComponentTypes(); populateSensorTypes();" disabled="${editMode}">
										<form:option value="">${emptySelectMessage}</form:option>
										<form:options items="${providers}" itemValue="value" itemLabel="label" />
									</form:select>
									<c:if test="${editMode}">
										<form:hidden path="providerId" />
									</c:if>
									<form:errors path="providerId" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="componentType" class="control-label">
									<spring:message code="alertrule.componentType" />
								</form:label>
								<div class="controls">
									<form:select path="componentType">
										<form:option value=""></form:option>
										<form:options items="${componentTypes}" itemValue="value" itemLabel="label" />
									</form:select>
									<form:errors path="componentType" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							<div class="control-group">
								<form:label path="sensorType" class="control-label">
									<spring:message code="alertrule.sensorType" />
								</form:label>
								<div class="controls">
									<form:select path="sensorType">
										<form:option value=""></form:option>
										<form:options items="${sensorTypes}" itemValue="value" itemLabel="label" />
									</form:select>
									<form:errors path="sensorType" cssClass="text-error" htmlEscape="false" />
								</div>
							</div>
							
							<div id="expressionFields">
								<h1 class="lead">
									<spring:message code="alertrule.expression.title" />
									<br />
								</h1>
								<div class="control-group">
									<form:label path="trigger" class="control-label">
										<spring:message code="alertrule.trigger" />
									</form:label>
									<div class="controls">
										<form:select path="trigger" id="trigger">
											<form:options items="${alertRuleTriggers}" itemValue="value" itemLabel="label" />
										</form:select>
										<form:errors path="trigger" cssClass="text-error" htmlEscape="false" />
									</div>
								</div>
								<div class="control-group">
									<form:label path="expression" class="control-label">
										<spring:message code="alertrule.expression" />
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
										<a href="#confirmModal" role="button" class="btn btn-primary" onclick="showConfirmCreateRuleModalWindow()">
											<spring:message code="alertrule.button.confirm"/>
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

<%@include file="/WEB-INF/jsp/alertRule/confirmModal.jsp"%>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>