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
         <div class=""><h1 class="lead"><spring:message code="sensor.section.title"/><br><small><spring:message code="sensor.sendData.title"/> </small></h1>
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
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="sensor.provider" /></strong></div>
						    		<div class="span8" id="providerId">${virtualSensor.providerId}</div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="sensor.id" /></strong></div>
						    		<div class="span8" id="sensorId">${virtualSensor.sensorId}</div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="sensor.tokenAuth" /></strong></div>
						    		<div class="span8" id="tokenAuth">${virtualSensor.tokenAuth}</div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="sensor.dataType" /></strong></div>
						    		<div class="span8" id="dataType">${virtualSensor.dataType}</div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="sensor.numOfIterations" /></strong></div>
						    		<div class="span8" id="numOfIterations">${virtualSensor.numOfIterations}</div>
						    	</div>
						    	<div class="row-fluid">
						    		<div class="span4"><strong><spring:message code="sensor.freq" /></strong></div>
						    		<div class="span8" id="freq">${virtualSensor.freq}</div>
						    	</div>
						    	<c:if test="${virtualSensor.dataType eq 'TEXT'}">
						    		<div class="row-fluid">
							    		<div class="span4"><strong><spring:message code="sensor.value" /></strong></div>
							    		<div class="span8" id="value">${virtualSensor.value}</div>
							    	</div>
							    </c:if>
                           </div>
                       </div>
                   </div>

               </div>
               
              <div class="accordion" id="accordion2">
				<div class="accordion-group">
					<div class="accordion-heading">
						<a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion2" href="#detailAccordionCollapse2">
							<i class="icon-th"></i>
							<spring:message code="sensor.subsection.enviamentDades"/>
							<i class="icon-chevron-down pull-right"></i>
						</a>
					</div>
					<div id="detailAccordionCollapse2" class="accordion-body collapse in">
						<div class="accordion-inner">
							<section id="content"></section>
				       </div>
	                </div>
                </div>
        	</div>
    	</div>
	</div>
</div>

<script>
$(document).ready(function($){   
	var sensorId = $('#sensorId').text();
    var providerId = $('#providerId').text();
    var dataType = $('#dataType').text();
    var freq = $('#freq').text();
    var numOfIterations = $('#numOfIterations').text();
    var value = $('#value').text();
    var tokenAuth = $('#tokenAuth').text();
    var iteration = 0;
    
	$('#content').append('<strong>Inici de transmissió de dades</strong> <br>');
	$('#content').append('<hr>');
    
	setTimeout(sendData, 0);

	function sendData(){
		
		iteration = iteration + 1 ;
		
		var url = 'send/'+sensorId+'/'+providerId+'/'+tokenAuth+'/';
		var valueToSend='';
		
		if(dataType == 'TEXT'){	
			valueToSend = value+'_'+aleatorio(0,10);
		}else if(dataType == 'NUMBER'){
			valueToSend = aleatorio(0,100);
		}else{
			var valueTo = aleatorio(0,2);
			if(valueTo == 0){
				valueToSend = 'false';
			}else{
				valueToSend = 'true';
			}
		}
		
		$.post(url + valueToSend)
    	.success(function(data) {

    		if(data == 'OK'){
	    		$('#content').append('Enviant dada: '+ valueToSend + '<br>');
	    			if (numOfIterations == iteration) {
	    				$('#content').append('<hr>');
	    				$('#content').append('<strong>Fi de transmissió de dades</strong>');
	    			}
    		}else{
    			$('#content').append('<div class="alert alert-error"><strong>Problemes en la transmissió: </strong><br><br>'
    									+ data +
    									'<br><br><strong>Es cancel·la la transmissió de dades</strong></div>');
    			numOfIterations = iteration;
    		}
    	});
		
		if (numOfIterations > iteration) { 
			setTimeout(sendData, freq);
		}
	}   
	
	function aleatorio(inferior,superior){
	    numPosibilidades = superior - inferior;
	    aleat = Math.random() * numPosibilidades;
	    aleat = Math.floor(aleat);
	    return parseInt(inferior) + aleat;
	} 
});
</script>

</div>
</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp" %>