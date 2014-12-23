<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@include file="/WEB-INF/jsp/common/include_script_graphics.jsp"%>

<spring:url value="/admin/sensor/lastObs/${sensor.id}/" var="lastObservationsRESTURL" />



<script type="text/javascript">
$(document).ready(function() {
	
	<c:if test="${sensor.dataType == 'NUMBER'}">
		makeNumberChart('#plot', '${lastObservationsRESTURL}', '${sensor.unit}');
	</c:if>
	
	<c:if test="${sensor.dataType == 'BOOLEAN'}">
		makeBooleanChart('#plot', '${lastObservationsRESTURL}', '${sensor.unit}');
	</c:if>

	<c:if test="${sensor.dataType == 'TEXT'}">
		makeTextChart('#plot', '${lastObservationsRESTURL}', '${sensor.unit}');
	</c:if>
});
</script>

<div class="row-fluid">
	<div class="span12">
		<div class="accordion" id="lastDataAccordion">
			<div class="accordion-group">
				<div class="accordion-heading">
					<a class="accordion-toggle" data-toggle="collapse" data-parent="#lastDataAccordion"
						href="#lastDataAccordionCollapse"> <i class="icon-time "></i> <spring:message
							code="sensor.observation.lastData" /> <i class="icon-chevron-down pull-right"></i> </a>
				</div>
				<div id="lastDataAccordionCollapse" class="accordion-body collapse in">
					<div class="accordion-inner">
						<div class="container-fluid">
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.dataType" /> </strong>
								</div>
								<div class="span8">
									<span class="label"><spring:message code="sensor.dataType.${sensor.dataType}" /> </span>
								</div>
							</div>
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.unit" /> </strong>
								</div>
								<div class="span8">${sensor.unit}</div>
							</div>
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.observation.value" /> </strong>
								</div>
								<div class="span8">${sensorLastObservation.value}</div>
							</div>
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.observation.timestamp" /> </strong>
								</div>
								<div class="span8">
									<c:if test="${not empty sensorLatObservation}">
										<spring:eval expression="sensorLastObservation.date" />
									</c:if>
								</div>
							</div>
						</div>
					</div>
				</div>
			</div>
			<br />
			<div class="row-fluid">
				<div class="span12">
					<div class="control-group pull-right">
						<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<div class="row-fluid">
	<div class="span12">
		<div class="accordion" id="activityAccordion">
			<div class="accordion-group">
				<div class="accordion-heading">
					<a class="accordion-toggle" data-toggle="collapse" data-parent="#activityAccordion"
						href="#activityAccordionCollapse"> <i class="icon-signal "></i> <spring:message
							code="sensor.observation.activity" /> <i class="icon-chevron-down pull-right"></i> </a>
				</div>
				<div id="activityAccordionCollapse" class="accordion-body collapse in">
					<div class="accordion-inner">
						<div class="observation-graph" id="plot"></div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
