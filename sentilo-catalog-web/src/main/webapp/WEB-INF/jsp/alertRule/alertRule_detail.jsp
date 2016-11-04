<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/alertRule/list?nameTableRecover=alertRuleTable&fromBack=true" var="backURL" />
<spring:url value="/admin/alertRule/${alertRule.id}/reapply" var="actionURL" />

<spring:eval var="showAdminControls" expression="T(org.sentilo.web.catalog.security.SecurityUtils).showAdminControls('READ', alertRule)"/>

<%@include file="/WEB-INF/jsp/alertRule/confirmModal.jsp"%>

<script>

	function showConfirmReappplyRuleModalWindow() {
		showConfirmModal('${alertRule.providerId}','${alertRule.componentType}','${alertRule.sensorType}');
	}
	
</script>

<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">

				<div class="row-fluid">
					<div class="span12">

						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">${alertRule.name}</h1>

						<div class="tabbable">
							<ul class="nav nav-tabs">
								<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="alertrule.detail.title" /> </a></li>
							</ul>
							
							<div class="tab-content">
								<div class="tab-pane active" id="tab1">
									<div class="accordion" id="detailAccordion">
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion"
													href="#detailAccordionCollapse"> <i class="icon-th"></i> <spring:message code="data" /> <i
													class="icon-chevron-down pull-right"></i> </a>
											</div>
											<div id="detailAccordionCollapse" class="accordion-body collapse in">
												<div class="accordion-inner">
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.id" /> </strong>
														</div>
														<div class="span8">${alertRule.id}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.name" /> </strong>
														</div>
														<div class="span8">${alertRule.name}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.description" /> </strong>
														</div>
														<div class="span8">${alertRule.description}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.createdAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="alertRule.createdAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.updatedAt" /> </strong>
														</div>
														<div class="span8">
															<spring:eval expression="alertRule.updatedAt" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.providerId" /> </strong>
														</div>
														<div class="span8">${alertRule.providerId}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.componentType" /> </strong>
														</div>
														<div class="span8">${alertRule.componentType}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.sensorType" /> </strong>
														</div>
														<div class="span8">${alertRule.sensorType}</div>
													</div>
												</div>
											</div>
										</div>
									</div>
									<div class="accordion" id="expressionAccordion">
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#expressionAccordion"
													href="#expressionAccordionCollapse"> <i class="icon-th"></i> <spring:message
														code="alertrule.expression.title" /> <i class="icon-chevron-down pull-right"></i> </a>
											</div>
											<div id="expressionAccordionCollapse" class="accordion-body collapse in">
												<div class="accordion-inner">
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.trigger" /> </strong>
														</div>
														<div class="span8">
															<spring:message code="alert.trigger.${alertRule.trigger}" />
														</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.expression" /> </strong>
														</div>
														<div class="span8">${alertRule.expression}</div>
													</div>
												</div>
											</div>
										</div>
									</div>
									
									<div class="accordion" id="resultAccordion">
										<div class="accordion-group">
											<div class="accordion-heading">
												<a class="accordion-toggle" data-toggle="collapse" data-parent="#resultAccordion"
													href="#resultAccordionCollapse"> <i class="icon-th"></i> <spring:message
														code="alertrule.result.title" /> <i class="icon-chevron-down pull-right"></i> </a>
											</div>
											<div id="resultAccordionCollapse" class="accordion-body collapse in">
												<div class="accordion-inner">
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.totalSensors" /> </strong>
														</div>
														<div class="span8">${alertRule.totalSensors}</div>
													</div>
													<div class="row-fluid">
														<div class="span4">
															<strong><spring:message code="alertrule.generatedAlerts" /> </strong>
														</div>
														<div class="span8">${alertRule.generatedAlerts}</div>
													</div>
												</div>
											</div>
										</div>
									</div>
									
									<div class="row-fluid">
										<div class="span12">
											<div class="control-group pull-right">
												<%@include file="/WEB-INF/jsp/common/include_input_back.jsp"%>
												<c:if test="${showAdminControls}">
												<button id="confirmContinueButton" class="btn btn-primary" onclick="showConfirmReappplyRuleModalWindow();">
													<spring:message code="alertrule.button.reapply"/>
												</button>
												</c:if>
											</div>
										</div>
									</div>
								</div>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<form:form method="post" modelAttribute="alertRule" action="${actionURL}">
	<form:hidden path="id" />
</form:form>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>