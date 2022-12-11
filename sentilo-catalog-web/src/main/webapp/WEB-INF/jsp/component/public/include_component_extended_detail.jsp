<%@page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<c:set var="extendedDetailUrlValue" value="${component.extendedDetailUrl}" />


<div class="container-fluid">
    <div class="content">
        <div id="component-detail-header" class="row-fluid">
            <div class="span12" >
                <%@ include file="/WEB-INF/jsp/component/public/include_component_detail_header.jsp"%>
            </div>
        </div>
                
       	<iframe src="${extendedDetailUrlValue}" style='height: 800px; width: 100%;' frameborder="0" scrolling="no" id="extendedDetailUrlIframe"></iframe>
        
        <c:if test="${showFooterLegal eq 'true'}">
			<div class="footer-legal">
				<span><spring:message code="footer.legal" /></span>
			</div>
        </c:if>
    </div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp"%>

<script>
	// Selecting the iframe element
	var iframe = document.getElementById("extendedDetailUrlIframe");
	
	// Adjusting the iframe height onload event
	iframe.onload = function(){
	    iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
	}
</script>
