<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', sensor)"/>

<div class="tab-content">
	<div class="${tab1PaneClass}" id="tab1">
		<div class="accordion" id="detailAccordion">
			<div class="accordion-group">
				<div class="accordion-heading">
					<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion" href="#detailAccordionCollapse">
						<i class="icon-th"></i> <spring:message code="data" /> <i class="icon-chevron-down pull-right"></i> </a>
				</div>
				<div id="detailAccordionCollapse" class="accordion-body collapse in">
					<div class="accordion-inner">
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.tenantId" /> </strong>
							</div>
							<div class="span8">${sensor.tenantId}</div>
						</div>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.sensorId" /> </strong>
							</div>
							<div class="span8">${sensor.sensorId}</div>
						</div>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.description" /> </strong>
							</div>
							<div class="span8">${sensor.description}</div>
						</div>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.componentId" /> </strong>
							</div>
							<div class="span8">${sensor.componentId}</div>
						</div>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.accessType" /> </strong>
							</div>
							<div class="span8">
								<c:choose>	
									<c:when test="${sensor.publicAccess}">
										<spring:message code="public" />
									</c:when>
									<c:otherwise>
										<spring:message code="private" />
									</c:otherwise>
								</c:choose>
							</div>
						</div>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.createdAt" /> </strong>
							</div>
							<div class="span8">
								<spring:eval expression="sensor.createdAt" />
							</div>
						</div>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.updatedAt" /> </strong>
							</div>
							<div class="span8">
								<spring:eval expression="sensor.updatedAt" />
							</div>
						</div>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.type" /> </strong>
							</div>
							<div class="span8">
								<span class="label label-info">${sensor.type}</span>
							</div>
						</div>
						<c:set value="${sensor.tagsAsList}" var="tags" scope="request" />
						<%@include file="/WEB-INF/jsp/common/include_detail_tags.jsp"%>
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.dataType" /> </strong>
							</div>
							<div class="span8">
								<span class="label label-info"><spring:message code="sensor.dataType.${sensor.dataType}" /> </span>
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
								<strong><spring:message code="sensor.timeZone" /> </strong>
							</div>
							<div class="span8">${sensor.timeZone}</div>
						</div>							
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.state" /> </strong>
							</div>
							<div class="span8">
								<span class="label label-info"><spring:message code="sensor.state.${sensor.state}" /> </span>
							</div>
						</div>
						<c:if test="${not empty  sensor.substate}">
							<div class="row-fluid">
								<div class="span4">
									<strong><spring:message code="sensor.substate" /> </strong>
								</div>
								<div class="span8">
									<span class="label label-info"><c:out value="${sensor.substateDesc}"/> </span>
								</div>
							</div>
						</c:if>		
						<div class="row-fluid">
							<div class="span4">
								<strong><spring:message code="sensor.ttl" /> </strong>
							</div>
							<div class="span8">${sensor.ttl}</div>
						</div>										
				</div>
			</div>
		</div>
		<br />
		<div class="row-fluid">
			<div class="span12">
				<div class="control-group  pull-right">
					<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
					<c:if test="${showAdminControls and not sensor.federatedResource}">
					<a href="${editSensorLink}" class="btn btn-primary"> <spring:message code="sensor.edit.title" /> </a>
					</c:if>
				</div>
			</div>
		</div>
	</div>
	</div>
	<div class="${tab2PaneClass}" id="tab2">
		<div class="accordion" id="technicalDetailsAccordion">
		    <c:set var="technicalDetails" value="${sensor.technicalDetails}" />
			<c:set var="resourceIsComponent" value="false" />
			<%@include file="/WEB-INF/jsp/common/include_technical_details.jsp"%>
			<br />
			<div class="row-fluid">
				<div class="span12">
					<div class="control-group  pull-right">
						<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
						<c:if test="${showAdminControls and not sensor.federatedResource}">
						<a href="${editSensorLink}" class="btn btn-primary"> <spring:message code="sensor.edit.title" /> </a>
						</c:if>
					</div>
				</div>
			</div>
		</div>
	</div>
	<div class="${tab3PaneClass}" id="tab3">
		<div class="accordion" id="detailAdditionalInfoAccordion">
		    <c:set var="visualConfiguration"  value="${sensor.visualConfiguration}" />
		    <c:set var="isSensorConfiguration" value="true" />
			<%@include file="/WEB-INF/jsp/common/include_visual_configuration.jsp"%>
			<br />
			<div class="row-fluid">
				<div class="span12">
					<div class="control-group  pull-right">
						<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
						<c:if test="${showAdminControls and not sensor.federatedResource}">
						<a href="${editSensorLink}" class="btn btn-primary"> <spring:message code="sensor.edit.title" /> </a>
						</c:if>
					</div>
				</div>
			</div>
		</div>
	</div>
	<div class="${tab4PaneClass}" id="tab4">
		<div class="accordion" id="detailAdditionalInfoAccordion">
		    <c:set var="additionalInfo"  value="${sensor.additionalInfo}" />
			<%@include file="/WEB-INF/jsp/common/include_additional_info.jsp"%>
			<br />
			<div class="row-fluid">
				<div class="span12">
					<div class="control-group  pull-right">
						<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
						<c:if test="${showAdminControls and not sensor.federatedResource}">
						<a href="${editSensorLink}" class="btn btn-primary"> <spring:message code="sensor.edit.title" /> </a>
						</c:if>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>