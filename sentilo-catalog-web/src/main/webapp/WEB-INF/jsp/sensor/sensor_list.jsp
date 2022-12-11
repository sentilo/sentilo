<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval  var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('LIST', 'org.sentilo.web.catalog.domain.Sensor')"/>

<c:set var="sensorTable"  value="sensorTable" />

<spring:url value="/admin/sensor/list/json" var="sensorsAjaxSource" />
<spring:url value="/admin/sensor/delete" var="deleteURL" />
<spring:url value="/admin/sensor/new" var="newSensorURL" />
<spring:url value="/admin/sensor/delete" var="deleteSensorsURL" />
<spring:url value="/admin/sensor/changeAccessType" var="changeAccessTypeURL" />
<spring:url value="/admin/sensor/changeState" var="changeStateURL" />
<spring:url value="/admin/sensor/list/excel?tableName=${sensorTable}" var="sensorExcelSource" />


<spring:message code="sure.delete.sensor" var="deleteSensorConfirmMessage" />
<spring:message code="sure.change.accessType" var="changeAccessTypeConfirmMessage" />
<spring:message code="sure.change.sensorState" var="changeSensorStateConfirmMessage" />
<spring:message code="select.empty" var="emptySelectMessage" javaScriptEscape="false" htmlEscape="false"/>

<script type="text/javascript">

$(document).ready(function() {
	$('#substateFields').hide();
	$("#changeStateModal").modal("hide");
	
	$("#changeSensorStateBtn").click(function() {	
		resetChangeStateModalControls();
		$("#changeStateModal").modal("show");
	});
	$("#sendNewState").click(function() {		
		$("#changeStateModal").modal("hide");
		// Faltaria recuperar la lista de sensores seleccionados
		//jsonPOST("${sendIncidenceURLPrefix}${sendIncidenceURLSuffix}/", $("#changeStateModalForm").serializeObject(), null);		
	});	
});

function resetChangeStateModalControls(){
	$("#sensorState").val("online").change();
	controlDisplaySubstateFields();	
}

function controlDisplaySubstateFields(){			
	$('#sensorSubstate').prop('selectedIndex', 0);
	$('#substateFields').show();
	
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
							<spring:message code="sensor.list.title" />
							<br />
						</h1>

						<%@include file="/WEB-INF/jsp/common/include_sensor_list.jsp"%>
						
						<c:if test="${showAdminControls}">
							<div class="control-group pull-right">
								<a href="#" onclick="deleteSelected('sensors','<spring:escapeBody>${deleteSensorConfirmMessage} </spring:escapeBody>');" class="btn btn-danger"> 
									<spring:message	code="sensor.delete.title" /> 
								</a>
								<a href="#" id="changeSensorStateBtn" class="btn">							
									<spring:message code="button.state.change" />  
								</a> 
								<a href="#" onclick="changeAccessType('sensors', '<spring:escapeBody>${changeAccessTypeConfirmMessage}</spring:escapeBody>', 'public', '${changeAccessTypeURL}');" class="btn">							
									<spring:message code="button.accessType.change.toPublic" /> 
								</a>
								<a href="#" onclick="changeAccessType('sensors','<spring:escapeBody>${changeAccessTypeConfirmMessage}</spring:escapeBody>', 'private', '${changeAccessTypeURL}');" class="btn">
									<spring:message code="button.accessType.change.toPrivate" /> 
								</a>							
								<a href="#" onclick="window.location.href='${newSensorURL}';" class="btn"> <spring:message code="sensor.new.title" /> </a>
							</div>
						</c:if>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<!--  SensorState change modal window -->
<div id="changeStateModal" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="changeStateModalLabel" aria-hidden="true">
	<div class="modal-header">
		<button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
		<h3 id="changeStateModalLabel"><spring:message code="sensor.list.change_state.modal.title" /></h3>
	</div>
	<div class="modal-body">
		<div id="changeStateModalInfo" class="alert alert-info"><spring:message code="sensor.list.change_state.modal.help" /></div>	
		<form id="changeStateModalForm" class="form-horizontal" onsubmit="return false">						
				<div class="control-group">
					<label class="control-label" for="sensorState"> <spring:message code="sensor.state" /> </label>					
					<div class="controls">
						<select id="sensorState" onchange="controlDisplaySubstateFields();" name="sensorState">
							<c:forEach items="${sensorStates}" var="sensorState">
								<option value="${sensorState.value}">${sensorState.label}</option>
							</c:forEach>
						</select>											
					</div>
				</div>
				
				<div class="control-group" id="substateFields">
					<label class="control-label" for="sensorSubstate"> <spring:message code="sensor.substate" /> </label>					
					<div class="controls">																		
						<select id="sensorSubstate" name="sensorSubstate">
							<option value="">${emptySelectMessage}</option>
							<c:forEach items="${sensorSubstates}" var="sensorSubstate">
								<option value="${sensorSubstate.code}">${sensorSubstate.description}</option> 																
							</c:forEach>
						</select>											
					</div>
				</div>											
		</form>
	</div>
	<div class="modal-footer">
		<button class="btn" data-dismiss="modal" aria-hidden="true"><spring:message code="button.cancel"/></button>
		<button id="sendNewState" class="btn btn-primary" onclick="changeSensorsState('sensors', '<spring:escapeBody>${changeSensorStateConfirmMessage}</spring:escapeBody>', '${changeStateURL}');"><spring:message code="button.confirm"/></button>
	</div>
</div>


<%@include file="/WEB-INF/jsp/common/footer.jsp"%>
