<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="container-fluid">
    <div class="content">
    <div class="row-fluid">

        <div id="menu-left" class="span3">

            <div class="sidebar-nav">
                <ul class="nav nav-list">
                    <li><a href="${linkSubscribeSensor}" class="current" data-original-title=""><i class="icon-signal"></i><span><spring:message code="menu.subscription.sensor" /></span></a></li>
                    <li><a href="${linkAlarmSubs}" data-original-title=""><i class="icon-warning-sign"></i><span> <spring:message code="menu.subscription.alarm" /></span></a></li>
                    <li><a href="${linkOrderSubs}" data-original-title=""><i class="icon-hand-right"></i><span><spring:message code="menu.subscription.order" /></span></a></li>
                </ul>
            </div> 
            <br>
        </div>

		<%@include file="/WEB-INF/jsp/common/messages.jsp" %>

        <div id="widget" class="span9">
          <div class="row-fluid">
       
        <div class="span12"> <br>
         <div class="pull-right logo"></div><br><br>   
         <div class=""><h1 class="lead"><spring:message code="sendData.section.title"/><br><small><spring:message code="sensor.recoverdata.title"/></small></h1>
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
                                    <spring:message code="sendData.subsection.sensor"/>
                                    <i class="icon-chevron-down pull-right"></i>
                                </a>
                            </div>
                            <div id="detailAccordionCollapse" class="accordion-body collapse in">
                            
                                <div class="accordion-inner">

								<form:form method="post" modelAttribute="externalSensor" action="recoverData" class="form-horizontal">
									<fieldset>
									<legend><spring:message code="subsection.remoteData" /></legend>
										<div class="control-group">
											<form:label path="providerId" class="control-label">
												<spring:message code="sensor.provider" />
											</form:label>
											<div class="controls">
												<form:input type="text" path="providerId" id="providerId" />
												<form:errors path="providerId" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="id" class="control-label">
												<spring:message code="sensor.id" />
											</form:label>
											<div class="controls">
												<form:input path="id" />
												<form:errors path="id" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
										<div class="control-group">
											<form:label path="number" class="control-label">
												<spring:message code="sensor.number" />
											</form:label>
											<div class="controls">
												<div class="input-append">
													<form:input type="text" path="number" id="number" />
								
												</div>
												<form:errors path="number" cssClass="text-error" htmlEscape="false" />
											</div>
										</div>
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
												<a href="#" type="button" id="recoverBtn" class="btn"> 
													<i class="icon-plus-sign icon-white"></i> <spring:message code="button.recoverdata"/>
												</a>
												<a href="#" type="button" id="subscribeBtn" class="btn">
													<i class="icon-plus-sign icon-white"></i> <spring:message code="button.subscribedata" />
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

<script>
$('#recoverBtn').on('click', function(event){
	 $("form#externalSensor").attr("action", "recoverData");
	 $('form#externalSensor').submit();
});
$('#subscribeBtn').on('click', function(event){
	 $("form#externalSensor").attr("action", "subscribeData");
	 $('form#externalSensor').submit();
});
</script>

</div>
</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp" %>