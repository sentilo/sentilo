<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/alarm/${alarm.id}/edit" var="editAlarmLink"/> 

<div class="container-fluid">
<div class="content">
<div class="row-fluid">
<div class="span3">
	<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp" %>
</div>
<div class="span9">

<div class="row-fluid">
<div class="span12">

	<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp" %>
	<%@include file="/WEB-INF/jsp/common/messages.jsp" %>
	
	<h1 class="lead">
		${alarm.name}<br/>
		<small><spring:message code="id"/> ${alarm.id}</small>
	</h1>
	
	<div class="tabbable">
		<ul class="nav nav-tabs">
			<li class="active"><a href="#tab1" data-toggle="tab"><spring:message code="alarm.detail.title" /></a></li>
		</ul>
		<div class="tab-content">
			<div class="tab-pane active" id="tab1">
				<div class="accordion" id="detailAccordion">
				<div class="accordion-group">
					<div class="accordion-heading">
						<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion" href="#detailAccordionCollapse">
							<i class="icon-th"></i>
							<spring:message code="data" />
							<i class="icon-chevron-down pull-right"></i>
						</a>
					</div>
					<div id="detailAccordionCollapse" class="accordion-body collapse in">
						<div class="accordion-inner">
					    	<div class="row-fluid">
					    		<div class="span4"><strong><spring:message code="alarm.description" /></strong></div>
					    		<div class="span8">${alarm.description}</div>
					    	</div>
					    	<div class="row-fluid">
					    		<div class="span4"><strong><spring:message code="alarm.createdAt" /></strong></div>
					    		<div class="span8"><spring:eval expression="alarm.createdAt" /></div>
					    	</div>
					    	<div class="row-fluid">
					    		<div class="span4"><strong><spring:message code="alarm.updatedAt" /></strong></div>
					    		<div class="span8"><spring:eval expression="alarm.updateAt" /></div>
					    	</div>
					    	<div class="row-fluid">
					    		<div class="span4"><strong><spring:message code="alarm.type" /></strong></div>
					    		<div class="span8"><span class="label"><spring:message code="alarm.type.${alarm.type}"/></span></div>
					    	</div>
					    	<c:if test="${alarm.type eq 'INTERNAL'}">
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="alarm.providerId" /></strong></div>
						    		<div class="span8">${alarm.providerId}</div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="alarm.componentId" /></strong></div>
						    		<div class="span8">${alarm.componentId}</div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="alarm.sensorId" /></strong></div>
						    		<div class="span8">${alarm.sensorId}</div>
						    	</div>
						    </c:if>
					    	<c:if test="${alarm.type eq 'EXTERNAL'}">
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="alarm.clientApplication" /></strong></div>
						    		<div class="span8">${alarm.clientApplication}</div>
						    	</div>
					    	</c:if>
						</div>
					</div>
				</div>
				</div>
		    	<c:if test="${alarm.type eq 'INTERNAL'}">

					<div class="accordion" id="expressionAccordion">
					<div class="accordion-group">
						<div class="accordion-heading">
							<a class="accordion-toggle" data-toggle="collapse" data-parent="#expressionAccordion" href="#expressionAccordionCollapse">
								<i class="icon-th"></i>
								<spring:message code="alarm.expression.title" />
								<i class="icon-chevron-down pull-right"></i>
							</a>
						</div>
						<div id="expressionAccordionCollapse" class="accordion-body collapse in">
							<div class="accordion-inner">
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="alarm.trigger" /></strong></div>
						    		<div class="span8"><spring:message code="alarm.trigger.${alarm.trigger}"/></div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="alarm.expression" /></strong></div>
						    		<div class="span8">${alarm.expression}</div>
						    	</div>
							</div>
						</div>
					</div>
					</div>					
				</c:if>
				<div class="row-fluid">
					<div class="span12">
						<div class="control-group pull-right">
							<%@include file="/WEB-INF/jsp/common/include_input_back.jsp" %>
							<a href="${editAlarmLink}" class="btn btn-primary">
								<spring:message code="alarm.edit.title"/>
							</a>
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
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>