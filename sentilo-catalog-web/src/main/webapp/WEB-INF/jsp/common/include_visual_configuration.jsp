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
			<div class="row-fluid">
				<div class="span4">
					<strong><spring:message code="visualConfiguration.timeZone" /> </strong>
				</div>
				<div class="span8">
					<c:if test="${not empty visualConfiguration and not empty visualConfiguration.timeZone}">
						<div class="span8">${visualConfiguration.timeZone}</div>
					</c:if>
				</div>
			</div>
			<div class="row-fluid">
				<div class="span4">
					<strong><spring:message code="visualConfiguration.dateFormatPattern" /> </strong>
				</div>
				<div class="span8">
					<c:if test="${not empty visualConfiguration and not empty visualConfiguration.dateFormatPattern}">
						<div class="span8">${visualConfiguration.dateFormatPattern}</div>
					</c:if>
				</div>
			</div>
			</c:if>
			<c:if test="${not isUserConfiguration}">
				<div class="row-fluid">
					<div class="span4">
						<strong><spring:message code="visualConfiguration.chartVisiblePointsNumber" /> </strong>
					</div>
					<c:if test="${not empty visualConfiguration and not empty visualConfiguration.chartVisiblePointsNumber}">
						<div class="span8">${visualConfiguration.chartVisiblePointsNumber}</div>
					</c:if>
				</div>
			</c:if>
		</div>
	</div>
</div>
