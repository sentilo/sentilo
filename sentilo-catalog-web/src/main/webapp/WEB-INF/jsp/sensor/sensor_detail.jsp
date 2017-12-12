<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_tab_classes.jsp"%>

<c:if test="${mode == 'detail' }">
	<c:set var="tabDetailClass" value=" class='active'" />
	<c:set var="tabDetailContentClass" value="active" />
	<c:set var="tabDataClass" value="" />
</c:if>
<c:if test="${mode == 'data' }">
	<c:set var="tabDetailClass" value="" />
	<c:set var="tabDataClass" value=" class='active'" />
</c:if>

<spring:url value="/admin/sensor/list?nameTableRecover=sensorTable&fromBack=true" var="backURL" />
<spring:url value="/admin/provider/${sensor.providerId}/detail" var="providerDetailURL" />
<spring:url value="/admin/provider/list" var="providerListURL" />
<spring:url value="/admin/sensor/list" var="sensorListURL" />
<spring:url value="/admin/sensor/${sensor.id}/edit" var="editSensorLink" />
<spring:url value="/admin/sensor/${sensor.id}/data" var="dataSensorLink" />
<spring:url value="/admin/sensor/${sensor.id}/detail" var="detailSensorLink" />

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				<%@include file="/WEB-INF/jsp/common/messages.jsp"%>					

				<h1 class="lead">
					${sensor.sensorId}<br /> <small><spring:message code="id" /> ${sensor.id}</small>
				</h1>

				<c:if test="${mode == 'detail' }">
					<ul class="nav nav-tabs">
						<li class="${tab1Class}"><a href="#tab1" data-toggle="tab"><spring:message code="sensor.detail.title" /></a></li>
						<li class="${tab2Class}"><a href="#tab2" data-toggle="tab"><spring:message code="technicalDetails.tab.label" /></a></li>
						<li class="${tab3Class}"><a href="#tab3" data-toggle="tab"><spring:message code="sensor.visualConfiguration" /></a></li>
						<li class="${tab4Class}"><a href="#tab4" data-toggle="tab"><spring:message code="sensor.additionalInfo" /></a></li>
						<li><a href="${dataSensorLink}"><spring:message code="sensor.data" /></a></li>
					</ul>
					<%@include file="include_sensor_detail.jsp"%>
				</c:if>
				<c:if test="${mode == 'data' }">
					<ul class="nav nav-tabs">
						<li><a href="${detailSensorLink}"><spring:message code="sensor.detail.title" /> </a></li>						
						<li><a href="${detailSensorLink}?openedTab=2"><spring:message code="technicalDetails.tab.label" /> </a></li>
						<li><a href="${detailSensorLink}?openedTab=3"><spring:message code="sensor.visualConfiguration" /></a></li>
						<li><a href="${detailSensorLink}?openedTab=4"><spring:message code="sensor.additionalInfo" /> </a></li>
						<li class="active"><a href="${dataSensorLink}"><spring:message code="sensor.data" /> </a></li>
					</ul>
					<%@include file="include_sensor_data.jsp"%>
				</c:if>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>