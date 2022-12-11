<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="select.empty" var="emptySelectMessage" javaScriptEscape="false" htmlEscape="false"/>
<div class="accordion-group">
	<div class="accordion-heading">
		<a class="accordion-toggle" data-toggle="collapse" data-parent="#technicalDetailsAccordion" href="#technicalDetailsAccordionCollapse"> 
			<i class="icon-th"></i> <spring:message code="data" /> 
			<i class="icon-chevron-down pull-right"></i> 
		</a>
	</div>
	<div id="technicalDetailsAccordionCollapse" class="accordion-body collapse in">		
		<div class="accordion-inner">
			<div class="control-group">
				<form:label path="technicalDetails.producer" class="control-label">
					<spring:message code="technicalDetail.producer" />
				</form:label>

				<div class="controls">
					<form:input path="technicalDetails.producer"  />
					<form:errors path="technicalDetails.producer" cssClass="text-error" htmlEscape="false" />
				</div>
			</div>
			<div class="control-group">
				<form:label path="technicalDetails.model" class="control-label">
					<spring:message code="technicalDetail.model" />
				</form:label>

				<div class="controls">
					<form:input path="technicalDetails.model"  />
					<form:errors path="technicalDetails.model" cssClass="text-error" htmlEscape="false" />
				</div>
			</div>
			<div class="control-group">
				<form:label path="technicalDetails.serialNumber" class="control-label">
					<spring:message code="technicalDetail.serialNumber" />
				</form:label>

				<div class="controls">
					<form:input path="technicalDetails.serialNumber"  />
					<form:errors path="technicalDetails.serialNumber" cssClass="text-error" htmlEscape="false" />
				</div>
			</div>
			<c:if test="${resourceIsComponent}">
				<div class="control-group">
					<form:label path="technicalDetails.macAddress" class="control-label">
						<spring:message code="technicalDetail.macAddress" />
					</form:label>
	
					<div class="controls">
						<form:input path="technicalDetails.macAddress"  />
						<form:errors path="technicalDetails.macAddress" cssClass="text-error" htmlEscape="false" />
					</div>
				</div>
			</c:if>
			<div class="control-group">
				<form:label path="technicalDetails.energy" class="control-label">
					<spring:message code="technicalDetail.energy" />
				</form:label>

				<div class="controls">
					<form:select path="technicalDetails.energy">
							<form:option value="">${emptySelectMessage}</form:option>
							<form:options items="${energyTypes}" itemValue="value" itemLabel="label" />
						</form:select>
					<form:errors path="technicalDetails.energy" cssClass="text-error" htmlEscape="false" />
				</div>
			</div>
			<c:if test="${resourceIsComponent}">
				<div class="control-group">
					<form:label path="technicalDetails.connectivity" class="control-label">
						<spring:message code="technicalDetail.connectivity" />
					</form:label>
	
					<div class="controls">
						<form:select path="technicalDetails.connectivity">
						    <form:option value="">${emptySelectMessage}</form:option>
							<form:options items="${connectivityTypes}" itemValue="value" itemLabel="label" />
						</form:select>						
						<form:errors path="technicalDetails.connectivity" cssClass="text-error" htmlEscape="false" />
					</div>
				</div>
			</c:if>																		
		</div>
	</div>
</div>
