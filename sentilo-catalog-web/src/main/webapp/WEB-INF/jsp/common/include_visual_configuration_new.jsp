<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="accordion-group">
	<div class="accordion-heading">
		<a class="accordion-toggle" data-toggle="collapse" data-parent="#visualConfigAccordion" href="#visualConfigAccordionCollapse"> 
			<i class="icon-th"></i> <spring:message code="visualConfiguration.accordion.head" /> 
			<i class="icon-chevron-down pull-right"></i> 
		</a>
	</div>
	<div id="visualConfigAccordionCollapse" class="accordion-body collapse in">		
		<div class="accordion-inner">
			<c:if test="${not isSensorConfiguration}">
				<div class="control-group">
					<form:label path="visualConfiguration.timeZone" class="control-label">
						<spring:message code="visualConfiguration.timeZone" />
					</form:label>
					<div class="controls">
						<form:input path="visualConfiguration.timeZone" />
						<form:errors path="visualConfiguration.timeZone" cssClass="text-error" htmlEscape="false" />
					</div>
				</div>
				<div class="control-group">
					<form:label path="visualConfiguration.dateFormatPattern" class="control-label">
						<spring:message code="visualConfiguration.dateFormatPattern" />
					</form:label>
					<div class="controls">
						<form:input path="visualConfiguration.dateFormatPattern" />
						<form:errors path="visualConfiguration.dateFormatPattern" cssClass="text-error" htmlEscape="false" />
					</div>
				</div>
			</c:if>
			<c:if test="${not isUserConfiguration}">
				<div class="control-group">
					<form:label path="visualConfiguration.chartVisiblePointsNumber" class="control-label">
						<spring:message code="visualConfiguration.chartVisiblePointsNumber" />
					</form:label>
					<div class="controls">
						<form:input path="visualConfiguration.chartVisiblePointsNumber"  />
						<form:errors path="visualConfiguration.chartVisiblePointsNumber" cssClass="text-error" htmlEscape="false" />
					</div>
				</div>
			</c:if>
		</div>
	</div>
</div>