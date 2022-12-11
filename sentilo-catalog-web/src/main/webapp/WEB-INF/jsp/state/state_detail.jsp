<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>


<spring:url value="/status/json" var="statusLink" />
<spring:url value="/static/js/sentilo/status.js" var="statusJS" />
<spring:url value="/static/js/loadingoverlay.min.js" var="loadingoverlayJS"/>

<script type="text/javascript" src="${loadingoverlayJS}"></script>
<script type="text/javascript" src="${statusJS}"></script>

<script type="text/javascript">
var globalPlatformMsgOk = '<spring:message code="status.global.platform.ok" />';
var globalPlatformMsgKo = '<spring:message code="status.global.platform.ko" />';

$(document).ready(function() {				
	$('#platformStatus').empty();	
	$.LoadingOverlay("show");
	
	$.getJSON('${statusLink}', function(data) {				
		$.LoadingOverlay("hide",true);
		refreshStatus(data); 
	});	
});
</script>

<script id="hidden-template" type="text/x-custom-template">
    <div class="row-fluid">
		<div class="span12">
			<div class="accordion" id="itemAccordion">
				<div class="accordion-group">
					<div class="accordion-heading">									 
						<a class="accordion-toggle" data-toggle="collapse" data-parent="#itemAccordion" href="#itemAccordionCollapse" title="##description##"> 
							<span class="alert-warning"><i class="##icon##"></i></span> ##name## <i class="icon-chevron-down pull-right"></i>   
						</a>										
					</div>
						
					<div id="itemAccordionCollapse" class="accordion-body collapse">
						<div class="accordion-inner ##desc-style##" id="itemStateDesc">##desc##</div>
					</div>
				</div>																	         
			 </div>
	     </div>
	</div>
</script>


<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span12">

				<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
				<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

				<h1 class="lead">					
					<br /> <small><spring:message code="status.title" /> </small>
				</h1>
				
				<div id="platformStatusPanel" class="media">
					<div class="row-fluid">
						<div class="span12">
							<div id="globalPlatformPanel">			                
			                    <i id="globalPlatformIconStatus"></i> <span id="globalPlatformMessage"></span>		                    		                    			                              
				            </div>
				        </div>
				    </div>       
	
					<div id="platformStatus"></div>
				</div>				
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>