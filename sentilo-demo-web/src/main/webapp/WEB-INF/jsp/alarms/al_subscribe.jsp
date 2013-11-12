<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="container-fluid">
    <div class="content">
    <div class="row-fluid">

        <div id="menu-left" class="span3">

            <div class="sidebar-nav">
                <ul class="nav nav-list">
                    <li><a href="${linkSubscribeSensor}" data-original-title=""><i class="icon-signal"></i><span><spring:message code="menu.subscription.sensor" /></span></a></li>
                    <li><a href="${linkAlarmSubs}" class="current" data-original-title=""><i class="icon-warning-sign"></i><span> <spring:message code="menu.subscription.alarm" /></span></a></li>
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
         <div class=""><h1 class="lead"><spring:message code="subscribe.section.title"/><br><small><spring:message code="alarm.subscribe.title"/></small></h1>
            </div>
        </div> 
         </div>
             <div class="row-fluid">
                <div class="span12">
                
                    <div class="accordion" id="detailAccordion">
                        <div class="accordion-group">
                            <div class="accordion-heading">
                                <a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion" href="#detailAccordionCollapse">
                                    <i class="icon-warning-sign"></i> 
                                    <spring:message code="alarm.subsection.alert"/>
                                    <i class="icon-chevron-down pull-right"></i>
                                </a>
                            </div>
                            <div id="detailAccordionCollapse" class="accordion-body collapse in">
                            
                                <div class="accordion-inner">

									<legend><spring:message code="subsection.remoteData" /></legend>
                                    <div class="row-fluid">
							    		<div class="span4"><strong><spring:message code="alarm.id" /></strong></div>
							    		<div class="span8" id="alarmId">${alarm.alarmId}</div>
							    	</div>
							    	<legend><spring:message code="subsection.authParameters" /></legend>	
							    	<div>
							    		<div class="span4"><strong><spring:message code="alarm.tokenAuth" /></strong></div>
							    		<div class="span8" id="tokenAuth">${alarm.tokenAuth}</div>
							    	</div>
							    			
			            		</div>
			                </div>
		                </div>
		        	</div>
		        	
		        	<div class="accordion" id="detailAccordion3">
                        <div class="accordion-group">
                            <div class="accordion-heading">
                                <a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAccordion3" href="#detailAccordionCollapse3">
                                    <i class="icon-th"></i> 
                                   	<spring:message code="alarm.recover.title"/>
                                    <i class="icon-chevron-down pull-right"></i>
                                </a>
                            </div>
                            <div id="detailAccordionCollapse3" class="accordion-body collapse in">
                            
                                <div class="accordion-inner">	 
			
							<div id="content"></div>
						</div>	
					</div>
				</div>
			</div>
		        	
    	</div>
	</div>
</div>
</div>

<spring:url value="/static/components/jquery-json/2.2/js/jquery.json-2.2.min.js" var="jsonScript"/>
<script type="text/javascript" src="${jsonScript}"></script>

<spring:url value="/static/components/jquery-websocket/0.0.1/js/jquery.websocket-0.0.1.js" var="websocketScript"/>
<script type="text/javascript" src="${websocketScript}"></script>

<script>
$(document).ready(function($){  
	var alarmId = $('#alarmId').text();
	var urlwebs = '${urlws}';
	var location = urlwebs+"/subscribe?type=alarm&alarmId="+alarmId; 
	
	var server = {
		connect : function() {
			this._ws = new WebSocket(location);
            this._ws.onopen = this._onopen;
            this._ws.onmessage = this._onmessage;
            this._ws.onclose = this._onclose;
		},

		 _onopen : function() {
             server._send('websockets are open for communications!');
     	 },

     	 _onmessage : function(event) {
             if (event.data) {
            	 $('#content').append(event.data + '<br>');
             }
	     },

	     _send : function(message) {
             if (this._ws)
                     this._ws.send(message);
	     },		    
	
	     _onclose : function(m) {
	             this._ws.close();
	             this._ws = null;
	             debug('Websocket connection closed. Reconnect');
	             server.connect();
	     }	
	};	

	server.connect();

});

</script>


</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp" %>