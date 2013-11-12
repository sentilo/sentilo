<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp" %>

<c:if test="${mode == 'edit' }">
	<spring:url value="/admin/alarm/${alarm.id}/edit" var="actionURL"/>
	<spring:message code="alarm.edit.title" var="pageTitle"/>
</c:if>
<c:if test="${mode == 'create' }">
	<spring:url value="/admin/alarm/create" var="actionURL"/>
	<spring:message code="alarm.new.title" var="pageTitle"/>
</c:if>

<c:set var="editMode" value="${mode == 'edit' }"/>

<spring:url value="/admin/sensor/search/json" var="sensorSearchURL"/>
<spring:url value="/admin/component/search/json" var="componentSearchURL"/>
<spring:url value="/admin/alarm/list" var="backURL"/>


<spring:message code="alarm.sensorId.instructions" var="sensorIdPlaceholder" javaScriptEscape="false" htmlEscape="false"/>
<spring:message code="alarm.sensorId.instructions.noproviderSelected" var="sensorIdPlaceholderNoProviderSelected" javaScriptEscape="false" htmlEscape="false"/>


<spring:message code="select.empty" var="emptySelectMessage"/>

<script type="text/javascript">
function toSuggestionList(data) {
	var result = [];
	$.each(data, function(index, sensor) {
		result.push(sensor.sensorId);
	});
	return result;
};


function emptyComponentSelect() {
	$('#componentId')
	    .find('option')
	    .remove()
	    .end()
	    .append('<option value="">${emptySelectMessage}</option>')
	    .val('');
};


function resetComponentSelect() {
	$("#compnentId").val($("#componentId option:first").val());
}

function resetProviderSelect() {
	$("#providerId").val($("#providerId option:first").val());
	emptyComponentSelect();
}

function changeSensorIdInputBehaviour(text, disabled) {
	var sensorIdInput = $('#sensorId'); 
	sensorIdInput.attr('placeholder', text);
	if (disabled) {
		sensorIdInput.val('');
	}
	sensorIdInput.prop('disabled', disabled);
};

function enableSensorIdInput() {
	changeSensorIdInputBehaviour('${sensorIdPlaceholder}', false);
};

function disableSensorIdInput() {
	changeSensorIdInputBehaviour('${sensorIdPlaceholderNoProviderSelected}', true);
}

function addComponentToSelect(select, component) {
	<c:if test="${mode == 'create' }">
		select.append('<option value="' + component.id + '">' + component.name +'</option>');
	</c:if>
	<c:if test="${mode == 'edit' }">
		if (component.id === '${alarm.componentId}') {
			select.append('<option value="' + component.id + '" selected>' + component.name +'</option>');
		} else {
			select.append('<option value="' + component.id + '">' + component.name +'</option>');
		}
	</c:if>
};

function populateComponents() {
	var providerId = $('#providerId').val();
	var selectComponentId = $('#componentId');
	emptyComponentSelect();
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
};

function populateSensorId() {
	var componentId = $('#componentId').val();
	if (componentId) { 
		enableSensorIdInput();
	} else {
		disableSensorIdInput();
	}
}

<spring:message code="alarm.error.provider.notselected" var="errorNoProviderSelected"/>

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

$(document).ready(function() {
	toggleAlarmType($('#alarmType').val());
	populateComponents();
	initSensorIdTypeahead();
});

function toggleAlarmType(type) {
	if (type === "EXTERNAL") {
		$('#clientApplicationFields').show();
		$('#componentIdFields').hide();
		$('#sensorIdFields').hide();
		$('#providerIdFields').hide();
		$('#expressionFields').hide();
		$('#notificationFields').hide();
		disableSensorIdInput();
		resetProviderSelect();
		resetComponentSelect();
	} else {
		$('#clientApplicationFields').hide();
		$('#sensorIdFields').show();
		$('#componentIdFields').show();
		$('#providerIdFields').show();
		$('#expressionFields').show();
		$('#notificationFields').show();
	}
};
</script>

<div class="container-fluid">
<div class="content">
<div class="row-fluid">
<div class="span3">
	<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp" %>
</div>
<div class="span9">

<div class="row-fluid">
<div class="span12">

	<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp" %>
	<%@include file="/WEB-INF/jsp/common/messages.jsp" %>

	<h1 class="lead">
		${pageTitle}<br/>
	</h1>

	<form:form method="post" modelAttribute="alarm" action="${actionURL}" class="form-horizontal" autocomplete="off">
		<div class="control-group">
			<form:label path="id" class="control-label">
				<spring:message code="alarm.id" />
			</form:label>
			<c:if test="${editMode}">				
				<form:hidden path="createdAt"/>
			</c:if>	 
			<div class="controls">				
				<form:input path="id" readonly="${editMode}"/>				
				<form:errors path="id" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>
		<div class="control-group">
			<form:label path="name" class="control-label">
				<spring:message code="alarm.name" />
			</form:label>
			<div class="controls">
				<form:input path="name" />
				<form:errors path="name" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>
		<div class="control-group">
			<form:label path="description" class="control-label">
				<spring:message code="alarm.description" />
			</form:label>
			<div class="controls">
				<form:textarea path="description" />
				<form:errors path="description" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>
		<div class="control-group">
			<form:label path="type" class="control-label">
				<spring:message code="alarm.type" />
			</form:label>
			<div class="controls">
				<form:select path="type" onchange="toggleAlarmType(this.value);" id="alarmType" disabled="${editMode}">
					<c:forEach items="${alarmTypes}" var="alarmType">
						<form:option value="${alarmType}">
							<spring:message code="alarm.type.${alarmType}"/>
						</form:option>
					</c:forEach>
				</form:select>
				<c:if test="${editMode}">
					<form:hidden path="type"/>
				</c:if>	
				<form:errors path="type" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>
		<div class="control-group" id="providerIdFields">
			<form:label path="providerId" class="control-label">
				<spring:message code="alarm.providerId" />
			</form:label>
			<div class="controls">
				<form:select path="providerId" id="providerId" onchange="populateComponents();" disabled="${editMode}">
					<form:option value="">${emptySelectMessage}</form:option>
					<form:options items="${providers}" itemValue="id" itemLabel="name" />
				</form:select>
				<c:if test="${editMode}">
					<form:hidden path="providerId"/>
				</c:if>
				<form:errors path="providerId" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>
		<div class="control-group" id="componentIdFields">
			<form:label path="componentId" class="control-label">
				<spring:message code="alarm.componentId" />
			</form:label>
			<div class="controls">
				<form:select path="componentId" id="componentId" onchange="populateSensorId();" disabled="${editMode}">
     				<form:option value=""><spring:message code="select.empty" /></form:option>
				</form:select>
				<c:if test="${editMode}">
					<form:hidden path="componentId"/>
				</c:if>
				<form:errors path="componentId" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>
		<div class="control-group" id="sensorIdFields">
			<form:label path="sensorId" class="control-label">
				<spring:message code="alarm.sensorId" />
			</form:label>
			<div class="controls">
				<form:input path="sensorId" id="sensorId" placeholder="${sensorIdPlaceholder}" readonly="${editMode}"/>
				<form:errors path="sensorId" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>
		<div class="control-group" id="clientApplicationFields">
			<form:label path="clientApplication" class="control-label">
				<spring:message code="alarm.clientApplication" />
			</form:label>
			<div class="controls">
				<form:select path="clientApplication" id="clientApplication" disabled="${editMode}">
					<c:forEach items="${applications}" var="application">
						<form:option value="${application.id}">
							${application.name}
						</form:option>
					</c:forEach>
				</form:select>	
				<c:if test="${editMode}">
					<form:hidden path="clientApplication"/>
				</c:if>		
				<form:errors path="clientApplication" cssClass="text-error" htmlEscape="false" />
			</div>
		</div>

		<div id="expressionFields">
			<h1 class="lead">
				<spring:message code="alarm.expression.title"/><br/>
			</h1>
			<div class="control-group">
				<form:label path="expression" class="control-label">
					<spring:message code="alarm.trigger" />
				</form:label>
				<div class="controls">
					<form:select path="trigger" id="trigger">
						<c:forEach items="${alarmTriggers}" var="alarmTrigger">
							<form:option value="${alarmTrigger}">
								<spring:message code="alarm.trigger.${alarmTrigger}"/>
							</form:option>
						</c:forEach>
					</form:select>
					<form:errors path="trigger" cssClass="text-error" htmlEscape="false" />
				</div>
			</div>
			<div class="control-group">
				<form:label path="expression" class="control-label">
					<spring:message code="alarm.expression" />
				</form:label>
				<div class="controls">
					<form:input path="expression" id="expression"/>
					<form:errors path="expression" cssClass="text-error" htmlEscape="false" />
				</div>
			</div>
		</div>
		

		<div class="control-group">
			<div class="controls">
				<a href="${backURL}" class="btn">
					<spring:message code="button.back"/>
				</a>
				<a href="#" onclick="$('form#alarm').submit();" class="btn btn-primary">
					<spring:message code="button.save"/>
				</a>
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