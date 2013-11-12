<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="taglibs.jsp"%>
<div class="navbar navbar-fixed-top">
	<div class="navbar-inner">
	    <div class="container-fluid">
	    	<a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
            	<span class="icon-bar"></span>
            	<span class="icon-bar"></span>
            	<span class="icon-bar"></span>
        	</a>
        	<div class="main">
      			<div class="logotype">
        			<a href="http://connecta.bcn.cat">
        			<spring:url value="/static/app/img/logo-ajment.png" var="imatgeConnecta"/>
        			<img width="130" height="43" alt="Logotip de l'Ajuntament de Barcelona. 
        			Enllaç a la pàgina principal del web de ConnectaBCN" src="${imatgeConnecta}"></a>
      			</div>
      
    		</div>
    		
		<spring:url value="/home" var="linkHome"/>
		<spring:url value="/virtualsensor/new" var="linkVirtualSensor" />
		<spring:url value="/alarm/new" var="linkAlarmCreate" />
		<spring:url value="/order/new" var="linkOrderCreate" />
		
		<spring:url value="/sensordata/subscribe" var="linkSubscribeSensor" />
		<spring:url value="/alarm/subscribe" var="linkAlarmSubs" />
		<spring:url value="/order/subscribe" var="linkOrderSubs" />
		
		<a class="brand" href="${linkHome}"><spring:message code="header.title.connecta"/><strong><spring:message code="header.title.demo"/></strong></a>		
		
		    <div class="nav-collapse">
		        <ul class="nav">
		          <li class="dropdown"><a data-toggle="dropdown" class="dropdown-toggle" href="#"><spring:message code="menu.send" /><b class="caret"></b></a>
		              <ul class="dropdown-menu">
		              
		                <li><a href="${linkVirtualSensor}" target="_blank"><spring:message code="menu.send.virtualSensor" /></a></li>
		                <li><a href="${linkAlarmCreate}" target="_blank"><spring:message code="menu.send.alarm" /></a></li>
		                <li><a href="${linkOrderCreate}" target="_blank"><spring:message code="menu.send.order" /></a></li>
		            </ul>
		       	 </li>
				 <li class="dropdown"><a data-toggle="dropdown" class="dropdown-toggle" href="#"><spring:message code="menu.subscription" /> <b class="caret"></b></a>
		              <ul class="dropdown-menu">
		                <li><a href="${linkSubscribeSensor}" target="_blank"><spring:message code="menu.subscription.sensor" /></a></li>
		                <li><a href="${linkAlarmSubs}" target="_blank"><spring:message code="menu.subscription.alarm" /></a></li>
		                <li><a href="${linkOrderSubs}" target="_blank"><spring:message code="menu.subscription.order" /></a></li>
		            </ul>
		        </li>
		    </ul>
    
   
		</div> <!--/.nav-collapse -->
		
		<div class="pull-right" id="lastMessages">
         <ul class="nav">
           <li class="dropdown"><a href="#" data-toggle="dropdown" class="dropdown-toggle"> <i class="icon-comment"></i></a> 
                <ul class="dropdown-menu pull-right" id="messageList">
                    <li><p>&nbsp;&nbsp; No data </p></li>
                </ul>
            </li>
        </ul>
    </div>  
    <spring:url value="/sensordata/lastMessages" var="lastMessagesUrl" />
    <script>
    
    $(document).ready(function($){  
    
		$('#lastMessages').on('mouseenter', function(event){
			reloadMessages();
		});
	    
		function reloadMessages(event) {
			$("#messageList").empty();
			var url = '${lastMessagesUrl}';
			$.post(url).success(function(data) {
					if(data.length > 0){
						for (var i=0; i<data.length; i++){
							if(data[i].type == 'eventData'){
				    			$('#messageList').append("<li><a href='#' data-original-title=''><span class='label label-info'>"+data[i].id+"</span> <i class='icon-info-sign'></i> value: "+data[i].value+"</a></li>");
							}else if(data[i].type == 'eventAlarm'){
				    			$('#messageList').append("<li><a href='#' data-original-title=''><span class='label label-important'>"+data[i].id+"</span> <i class='icon-warning-sign'></i> value: "+data[i].value+"</a></li>");
							}else{
				    			$('#messageList').append("<li><a href='#' data-original-title=''><span class='label label-success'>"+data[i].id+"</span> <i class='icon-bullhorn'></i> value: "+data[i].value+"</a></li>");
							}
						}
					}else{
		    			$('#messageList').append("<li> No data </li>");
	
					}
		   		}
	    	);
		};
    });
    
    </script>

		</div>
	</div>
</div>

