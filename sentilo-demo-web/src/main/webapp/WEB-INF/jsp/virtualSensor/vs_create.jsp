<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="container-fluid">
    <div class="content">
    <div class="row-fluid">

        <div id="menu-left" class="span3">

            <div class="sidebar-nav">
                <ul class="nav nav-list">
                    <li><a href="${linkVirtualSensor}" class="current" data-original-title=""><i class="icon-signal"></i><span><spring:message code="menu.send.virtualSensor" /></span></a></li>
                    <li><a href="${linkAlarmCreate}" data-original-title=""><i class="icon-warning-sign"></i><span> <spring:message code="menu.send.alarm" /></span></a></li>
                    <li><a href="${linkOrderCreate}" data-original-title=""><i class="icon-hand-right"></i><span><spring:message code="menu.send.order" /></span></a></li>
                </ul>
            </div> 
            <br>
        </div>

		<%@include file="/WEB-INF/jsp/common/messages.jsp" %>

        <div id="widget" class="span9">
          <div class="row-fluid">
       
        <div class="span12"> <br>
	         <div class="pull-right logo"></div><br><br>   
	         <div class=""><h1 class="lead"><spring:message code="sensor.section.title"/><br><small><spring:message code="sensor.new.title"/> </small></h1>
	            </div>
	        </div> 
         </div>
         
             <div class="row-fluid">
                <div class="span12">
                

                   	<div class="accordion" id="detailAccordion">
                        <div class="accordion-group">
                            <div class="accordion-heading">
                                <a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion" href="#detailAccordionCollapse">
                                    <i class="icon-signal "></i> 
                                    <spring:message code="sensor.subsection.virtualSensor"/>
                                    <i class="icon-chevron-down pull-right"></i>
                                </a>
                            </div>
                            <div id="detailAccordionCollapse" class="accordion-body collapse in">
                            
                                <div class="accordion-inner">
                                
                                    	<form:form method="post" modelAttribute="virtualSensor" action="registerAndSend" class="form-horizontal">
											<fieldset>
											<legend><spring:message code="sensor.subsection.configSensor" /></legend>																							
												<div class="control-group">
													<form:label path="sensorId" class="control-label">
														<spring:message code="sensor.id" />
													</form:label>
													<div class="controls">
														<form:input path="sensorId" />
														<form:errors path="sensorId" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>
												<div class="control-group">
													<form:label path="description" class="control-label">
														<spring:message code="sensor.description" />
													</form:label>
													<div class="controls">
														<form:textarea path="description" />
														<form:errors path="description" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>
												<div class="control-group">
													<form:label path="type" class="control-label">
														<spring:message code="sensor.type" />
													</form:label>
													<div class="controls">
														<form:select path="type">
															<c:forEach items="${sensorTypes}" var="type">
																<form:option value="${type}">
																	<spring:message code="sensor.type.${type}"/>
																</form:option>
															</c:forEach>
														</form:select>
														<form:errors path="type" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>												
											<legend><spring:message code="sensor.subsection.configComponent" /></legend>
												<div class="control-group">
														<form:label path="providerId" class="control-label">
															<spring:message code="sensor.provider" />
														</form:label>
														<div class="controls">
															<form:input path="providerId" />
															<form:errors path="providerId" cssClass="text-error" htmlEscape="false" />
														</div>
												</div>
												<div class="control-group">
														<form:label path="componentId" class="control-label">
															<spring:message code="sensor.component" />
														</form:label>
														<div class="controls">
															<form:input path="componentId" />
															<form:errors path="componentId" cssClass="text-error" htmlEscape="false" />
														</div>
												</div>
												<div class="control-group">
													<form:label path="type" class="control-label">
														<spring:message code="component.type" />
													</form:label>
													<div class="controls">
														<form:select path="componentType">
															<c:forEach items="${componentTypes}" var="componentType">
																<form:option value="${componentType}">
																	<spring:message code="component.type.${componentType}"/>
																</form:option>
															</c:forEach>
														</form:select>
														<form:errors path="componentType" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>	
												<div class="control-group">
													<form:label path="location" class="control-label">
														<spring:message code="sensor.location" />
													</form:label>
													<div class="controls">
														<form:input path="location" />
														<form:errors path="location" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>	
											<legend><spring:message code="sensor.subsection.configData" /></legend>
												<div class="control-group">
													<form:label path="dataType" class="control-label">
														<spring:message code="sensor.dataType" />
													</form:label>
													<div class="controls">
														<form:select path="dataType">
															<c:forEach items="${sensorDataTypes}" var="dataType">
																<form:option value="${dataType}">
																	<spring:message code="sensor.dataType.${dataType}"/>
																</form:option>
															</c:forEach>
														</form:select>
														<form:errors path="dataType" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>
												<div class="control-group">
													<form:label path="unit" class="control-label">
														<spring:message code="sensor.unit"/>
													</form:label>
													<div class="controls">
														<form:input path="unit" />
														<form:errors path="unit" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>
												<div class="control-group">
													<form:label path="numOfIterations" class="control-label">
														<spring:message code="sensor.numOfIterations" />
													</form:label>
													<div class="controls">
														<div class="input-append">
															<form:input type="text" path="numOfIterations" id="numOfIterations" />
														</div>
														<form:errors path="numOfIterations" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>
												<div class="control-group">
													<form:label path="freq" class="control-label">
														<spring:message code="sensor.freq" />
													</form:label>
													<div class="controls">
														<div class="input-append">
															<form:input type="text" path="freq" id="freq" />
														</div>
														<form:errors path="freq" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>
												<div class="control-group">
													<form:label path="value" class="control-label">
														<spring:message code="sensor.value" />
													</form:label>
													<div class="controls">
														<div class="input-append">
															<form:input type="text" path="value" id="value" />
														</div>
														<form:errors path="value" cssClass="text-error" htmlEscape="value" />
													</div>
												</div>
												
												<legend><spring:message code="subsection.authParameters" /></legend>
												<div class="control-group">
													<form:label path="tokenAuth" class="control-label">
														<spring:message code="sensor.tokenAuth" />
													</form:label>
													<div class="controls">
														<div class="input-append">
															<form:input type="text" path="tokenAuth" id="tokenAuth" />
														</div>
														<form:errors path="tokenAuth" cssClass="text-error" htmlEscape="false" />
													</div>
												</div>

												<div class="control-group">
													<div class="controls">
														<a href="#" type="button" id="registerBtn" class="btn"> 
															<i class="icon-plus-sign icon-white"></i> <spring:message code="button.registerandsend"/>
														</a>
														<a href="#" type="button" id="sendDataBtn" class="btn">
															<i class="icon-plus-sign icon-white"></i> <spring:message code="button.send" />
														</a>
													</div>
												</div>
											</fieldset>
										</form:form>
                                    
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

<script>
	$('#registerBtn').on('click', function(event){
		 $("form#virtualSensor").attr("action", "registerAndSend");
		 $('form#virtualSensor').submit();
	});
	$('#sendDataBtn').on('click', function(event){
		 $("form#virtualSensor").attr("action", "toSend");
		 $('form#virtualSensor').submit();
	});
</script>

<%@include file="/WEB-INF/jsp/common/footer.jsp" %>