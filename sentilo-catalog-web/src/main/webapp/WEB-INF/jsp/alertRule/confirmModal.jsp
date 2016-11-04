<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/alertRule/confirm/json" var="confirmURL" />
<spring:message code="alertrule.new.title" var="pageTitle" />
<spring:message code="alertrule.modal.confirm.body.empty" javaScriptEscape="true" htmlEscape="false" var="emptyResultMsg" />
<spring:message code="alertrule.modal.confirm.body" javaScriptEscape="true" htmlEscape="false" var="resultMsg"/>

<script type="text/javascript">

	function showConfirmModal(providerId, componentType, sensorType) {
		var numAlerts = 0;
		jsonGET('${confirmURL}?providerId=' + providerId + '&componentType=' + componentType + '&sensorType=' + sensorType, [], function(data) {
			numAlerts = data.length;
			updateConfirmModal(numAlerts);
			$('#confirmModal').modal('show');
		});	
	}
	
	function updateConfirmModal(numAlerts) {
		var message = '${emptyResultMsg}';
		if (numAlerts > 0) {
			message = '${resultMsg}';
			message = message.replace('{0}', numAlerts);
			$('#confirmContinueButton').show();
		} else {
			$('#confirmContinueButton').hide();
		}
		$('#confirmModal .modal-body').html('<p>'+message+'<p>');
	}
	
	function submitForm() {
		$('form#alertRule').submit();
	}

</script>

<!-- Confirm Modal -->
<div id="confirmModal" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="confirmModalLabel" aria-hidden="true">
	<div class="modal-header">
		<button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
		<h3 id="confirmModalLabel"><spring:message code="alertrule.modal.confirm.title" /></h3>
	</div>
	<div class="modal-body"></div>
	<div class="modal-footer">
		<button id="confirmCancelButton" class="btn" data-dismiss="modal" aria-hidden="true">
			<spring:message code="alertrule.button.cancel" />
		</button>
		<button id="confirmContinueButton" class="btn btn-primary" onclick="submitForm();">
			<spring:message code="alertrule.button.continue" />
		</button>
	</div>
</div>