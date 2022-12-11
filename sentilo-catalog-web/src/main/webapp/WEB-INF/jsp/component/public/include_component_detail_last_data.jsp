<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div id="accordion1" class="accordion">
	<div class="accordion-group">
		<div class="accordion-heading">
			<a href="#" class="accordion-toggle in"> <i class="icon-time "></i> <spring:message code="component.data.current" />
			</a>
		</div>
		<div class="accordion-body collapse in">
			<div class="accordion-inner accordion-tall" id="lastSensorDataPanel"></div>
		</div>
	</div>
</div>
