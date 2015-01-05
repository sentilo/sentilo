<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="accordion" id="photoAccordion">
	<div class="accordion-group">
		<div class="accordion-heading">
			<a class="accordion-toggle" data-toggle="collapse" data-parent="#photoAccordion" href="#photoAccordionCollapse">
				<i class="icon-camera"></i> <spring:message code="component.images" /> <i class="icon-chevron-down pull-right"></i>
			</a>
		</div>
		<div id="photoAccordionCollapse"
			class="accordion-body collapse <c:if test="${alternative ne 'true' or not empty testPage}">in</c:if>">
			<div class="accordion-inner paddind">
				<div id="foto">
					<c:choose>
						<c:when test="${ not empty component.photoUrl}">
							<img src="${component.photoUrl}">
						</c:when>
						<c:otherwise>
							<img src="../../static/img/NoPhotoFull.jpg">
						</c:otherwise>
					</c:choose>
				</div>
			</div>
		</div>
	</div>
</div>
