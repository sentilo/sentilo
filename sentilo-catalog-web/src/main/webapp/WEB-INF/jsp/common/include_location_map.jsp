<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="accordion" id="locationAccordion">
	<div class="accordion-group">
		<div class="accordion-heading">
			<a class="accordion-toggle" data-toggle="collapse" data-parent="#locationAccordion" href="#locationAccordionCollapse">
				<i class="icon-map-marker "></i> <spring:message code="location" /> <i class="icon-chevron-down pull-right"></i> </a>
		</div>

		<div id="locationAccordionCollapse"
			class="accordion-body collapse <c:if test="${alternative ne 'true' or not empty testPage}">in</c:if>">
			<div class="accordion-inner">
				<div id="map_canvas" style="width: 100%; height: 200px"></div>
			</div>
		</div>
	</div>
</div>