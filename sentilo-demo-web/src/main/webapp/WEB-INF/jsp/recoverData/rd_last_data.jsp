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
                            <spring:message code="sensor.subsection.virtualSensor"/>
							<i class="icon-chevron-down pull-right"></i>
						</a>
					</div>
					<div id="detailAccordionCollapse" class="accordion-body collapse in">
                                <div class="accordion-inner">
									<legend><spring:message code="subsection.remoteData" /></legend>
									<div class="row-fluid">
							    		<div class="span4"><strong><spring:message code="sensor.provider" /></strong></div>
							    		<div class="span8" id="providerId">${externalSensor.providerId}</div>
							    	</div>
							    	<div class="row-fluid">
							    		<div class="span4"><strong><spring:message code="sensor.id" /></strong></div>
							    		<div class="span8" id="sensorId">${externalSensor.id}</div>
							    	</div>
							    	<div class="row-fluid">
							    		<div class="span4"><strong><spring:message code="sensor.dataType" /></strong></div>
							    		<div class="span8" id="dataType">${externalSensor.dataType}</div>
							    	</div>
							    	<legend><spring:message code="subsection.authParameters" /></legend>
							    	<div class="row-fluid">
							    		<div class="span4"><strong><spring:message code="sensor.tokenAuth" /></strong></div>
							    		<div class="span8" id="tokenAuth">${externalSensor.tokenAuth}</div>
							    	</div>
								</div>
                       </div>
                   </div>

               </div>
               
              <div class="accordion" id="accordion2">
				<div class="accordion-group">
					<div class="accordion-heading">
						<a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion2" href="#detailAccordionCollapse2">
							<i class="icon-th"></i>
							<spring:message code="sensor.dataRecover.title"/>
							<i class="icon-chevron-down pull-right"></i>
						</a>
					</div>
					<div id="detailAccordionCollapse2" class="accordion-body collapse in">
						<div class="accordion-inner">
                            <ul>
                            <c:forEach items="${observation}" var="obs">
								<li><c:out value="${obs.value}"/></li>
							</c:forEach>
							</ul>
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

<%@include file="/WEB-INF/jsp/common/footer.jsp" %>