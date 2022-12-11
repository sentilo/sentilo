<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="accordion-group">
	<div class="accordion-heading">
		<a class="accordion-toggle" data-toggle="collapse" data-parent="#technicalDetailsAccordion" href="#technicalDetailsAccordionCollapse"> 
			<i class="icon-th"></i> <spring:message code="data" /> 
			<i class="icon-chevron-down pull-right"></i> 
		</a>
	</div>
	<div id="technicalDetailsAccordionCollapse" class="accordion-body collapse in">		
		<div class="accordion-inner">
			<div class="row-fluid">
				<div class="span4">
					<strong><spring:message code="technicalDetail.producer" /> </strong>
				</div>
				<div class="span8">${technicalDetails.producer}</div>
			</div>
			<div class="row-fluid">
				<div class="span4">
					<strong><spring:message code="technicalDetail.model" /> </strong>
				</div>
				<div class="span8">${technicalDetails.model}</div>
			</div>
			<div class="row-fluid">
				<div class="span4">
					<strong><spring:message code="technicalDetail.serialNumber" /> </strong>
				</div>
				<div class="span8">${technicalDetails.serialNumber}</div>
			</div>
			<c:if test="${resourceIsComponent}">
				<div class="row-fluid">
					<div class="span4">
						<strong><spring:message code="technicalDetail.macAddress" /> </strong>
					</div>
					<div class="span8">
						<span class="label label-info">${technicalDetails.macAddress}</span>
					</div>
				</div>
			</c:if>
			<div class="row-fluid">
				<div class="span4">
					<strong><spring:message code="technicalDetail.energy" /> </strong>
				</div>
				<div class="span8">
					<span class="label label-info">${technicalDetails.energy}</span>
				</div>
			</div>
			<c:if test="${resourceIsComponent}">
				<div class="row-fluid">
					<div class="span4">
						<strong><spring:message code="technicalDetail.connectivity" /> </strong>
					</div>
					<div class="span8">
						<span class="label label-info">${technicalDetails.connectivity}</span>
					</div>
				</div>					
			</c:if>	
		</div>
	</div>
</div>
