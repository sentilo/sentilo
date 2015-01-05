<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="accordion-group">
	<div class="accordion-heading">
		<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAdditionalInfoAccordion" href="#detailAdditionalInfoAccordionCollapse"> 
			<i class="icon-th"></i> <spring:message code="data" /> 
			<i class="icon-chevron-down pull-right"></i> 
		</a>
	</div>
	<div id="detailAdditionalInfoAccordionCollapse" class="accordion-body collapse in">
		<!-- AdditionalInfo is saved in a Map into the resource object, so to print field/value we must iterate over the map -->
		<div class="accordion-inner">
			<c:if test="${!empty additionalInfo}">
				<c:forEach var="entry" items="${additionalInfo}">
					<div class="row-fluid">
						<div class="span4">
							<strong><c:out value="${entry.key}" /> </strong>
						</div>
						<div class="span8">${entry.value}</div>
					</div>
				</c:forEach>
			</c:if>
		</div>
	</div>
</div>
