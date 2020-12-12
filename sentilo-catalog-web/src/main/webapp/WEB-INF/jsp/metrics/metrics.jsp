<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/admin/metrics/json" var="metricLink" />
<spring:url value="/static/js/sentilo/metrics.js" var="metricsJS" />


<script type="text/javascript" src="${chartJS}"></script>
<script type="text/javascript" src="${metricsJS}"></script>
<script type="text/javascript">

//main function
$(document).ready(function() {

	$.getJSON('${metricLink}', function(data) {
		main(data);
	});

	var interval = setInterval(function() {
		$.getJSON('${metricLink}', function(data) {
			 bindData(data);
		});
	}, 15000);
	
	$('#abstract').on('click','li',function(){ 
	    var id = $(this).attr('id');            
	    $("li").removeClass("active");        
	    $(this).addClass("active");
	    $('.tabcontent').hide();
	    $('#'+ id +"content").fadeIn('slow'); 
	    chartUpdate('#'+ id +"content");
	    $('#metrics-name small').text(id.replace('_', ' ').replace('/-/g', ' '));     
	      reorder();         
	  });
});
</script>


<div class="container-fluid">
	<div class="content">
		<div class="row-fluid">
			<div class="span3">
				<%@include file="/WEB-INF/jsp/common/include_sidebar.jsp"%>
			</div>
			<div class="span9">
				<div class="custom">
					<div class="span12">

						<%@include file="/WEB-INF/jsp/common/include_background_logo.jsp"%>
						<%@include file="/WEB-INF/jsp/common/messages.jsp"%>

						<h1 class="lead">
							<spring:message code="component.metrics" />
							<br />							
						</h1>				
						<div id="abstract" class="row-fluid">
		                    <ul></ul>
		                 </div>                      
		                 <div id="content" class="row-fluid">                        
		                    <div class="row-fluid tab-info" id="container">
		                      <h1 id="metrics-name"><small></small></h1> 
		                    </div>                       
		                 </div>                                                	                  	
		                
						<!-- dynamic content js -->	
							
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<%@include file="/WEB-INF/jsp/common/footer.jsp"%>