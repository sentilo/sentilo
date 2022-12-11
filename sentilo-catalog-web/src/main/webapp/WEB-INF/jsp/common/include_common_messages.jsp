<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<script type="text/javascript">

var catalogLocale= "${pageContext.response.locale}";

var falseValueMsg = '<spring:message code="false"/>';
var trueValueMsg = '<spring:message code="true"/>';
var sensorObsNotFoungMsg = '<spring:message code="component.sensor.observation.not.found"/>';
var emptyListMsg = '<spring:message code="empty.list"/>';
var locateMeMsg = '<spring:message code="locate.me"/>';

var latitudeErrorMsg = "<spring:message code='location.error.latitude' javaScriptEscape='true' htmlEscape='false'/>";
var longitudeErrorMsg = "<spring:message code='location.error.longitude' javaScriptEscape='true' htmlEscape='false'/>";
var locationDeleteAllConfirmMsg = "<spring:message code='location.button.delete.all.confirm' javaScriptEscape='true' htmlEscape='false'/>";

var deleteConfirmMsg = '<spring:message code="sure.delete" />';
var unassignConfirmMsg = '<spring:message code="sure.unassign" />';
var selectOneErrorMsg = '<spring:message code="select.one" />';
var okButtonLabelMsg = '<spring:message code="ok" />';

var sectorConfirmAssignedUsers = "<spring:message code='sector.users.assigned' />"; 
var sectorNoRowsSelected = "<spring:message code='sector.no.row.selected' />";
var sectorConfirmAssignedProviders = "<spring:message code='sector.provider.assigned' />";
var sectorConfirmAssignedApplications = "<spring:message code='sector.application.assigned' />";
var sectorGrantRead = "<spring:message code='sector.grant.R' />";
var sectorGrantAdmin = "<spring:message code='sector.grant.A' />";

var componentSeeMore = "<spring:message code='component.seeMore' />";
</script>