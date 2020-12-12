<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<script type="text/javascript">

var falseValueMsg = '<spring:message code="false"/>';
var trueValueMsg = '<spring:message code="true"/>';
var sensorObsNotFoungMsg = '<spring:message code="component.sensor.observation.not.found"/>';
var emptyListMsg = '<spring:message code="empty.list"/>';
var locateMeMsg = '<spring:message code="locate.me"/>';

var latitudeErrorMsg = "<spring:message code='location.error.latitude' javaScriptEscape='true' htmlEscape='false'/>";
var longitudeErrorMsg = "<spring:message code='location.error.longitude' javaScriptEscape='true' htmlEscape='false'/>";
var locationDeleteAllConfirmMsg = "<spring:message code='location.button.delete.all.confirm' javaScriptEscape='true' htmlEscape='false'/>";

var deleteConfirmMsg = '<spring:message code="sure.delete" var="deleteConfirmMessage" />';
var unassignConfirmMsg = '<spring:message code="sure.unassign" var="unassignConfirmMessage" />';
var selectOneErrorMsg = '<spring:message code="select.one" var="selectOneErrorMessage" />';
var okButtonLabelMsg = '<spring:message code="ok" var="okButtonLabel" />';

</script>