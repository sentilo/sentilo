<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<script type="text/javascript">
$.pnotify.defaults.history = false;

function showNotification(title, text, type, icon) {
	$.pnotify({
		title: title,
		text: text,
		hide: true,
		sticker: false,
		type: type,
		icon: icon
	});
}

function showConfirmNotification(title, text) {
	showNotification(title, text, 'success', 'icon-info-sign');
}


function showErrorNotification(title, text) {
	showNotification(title, text, 'error', 'icon-warning-sign');
}

</script>

<c:if test="${not empty confirmationMessage}">
	<script type="text/javascript">
		$(document).ready(function() {
			<spring:message code="confirm" var="title"/>
			<spring:message code="${confirmationMessage}" arguments="${confirmationMessageArgs}" var="msg" javaScriptEscape="true"/>
			showConfirmNotification('${title}', '${msg}');
		});
	</script>
</c:if>
<c:if test="${not empty errorMessage}">
	<script type="text/javascript">
		$(document).ready(function() {
			<spring:message code="error" var="title"/>
			<spring:message code="${errorMessage}" arguments="${errorMessageArgs}" var="msg" javaScriptEscape="true"/>
			showErrorNotification('${title}', '${msg}');
		});
	</script>
</c:if>
