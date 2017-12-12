<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="row-fluid component-chart-controls">
	<div class="span3">
		<strong><spring:message code="graphics.control.fromLabel" />: </strong> <span id="chartToDate"></span>
	</div>
	<div class="span3">
		<strong><spring:message code="graphics.control.toLabel" />: </strong> <span id="chartFromDate"></span>
	</div>
	<div class="span3">
		<strong><spring:message code="graphics.control.obsNumber" />: </strong> <span id="numObservations"></span> 
	</div>
	<div class="span3">
		<div class="pull-right">
			<div class="btn-toolbar btn-toolbar-nav-chart" style="margin: 0px; padding: 0px;">
				<div class="btn-group">
					<button class="btn" id="chartNavigateLeft" title="<spring:message code="graphics.control.nav.left" />"><i class="icon-chevron-left"></i></button>
					<button class="btn" id="chartNavigateRefresh" title="<spring:message code="graphics.control.nav.refresh" />"><i class="icon-refresh"></i></button>
					<button class="btn" id="chartNavigateRight" title="<spring:message code="graphics.control.nav.right" />"><i class="icon-chevron-right"></i></button>
				</div>
			</div>
		</div>
	</div>
</div>

