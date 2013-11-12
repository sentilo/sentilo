<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>
    
    <div class="container-fluid">
    <div class="content">
    <div class="row-fluid">

        <div id="widget" class="span12">
          <div class="row-fluid">
       
        <div class="span12"> <br>
	         <div class="pull-right logo"></div><br><br>   
	         <div class=""><h1 class="lead"><spring:message code="error_plattformjava" /><br><small><spring:message code="exception_details" /></small></h1>
	            </div>
	        </div> 
         </div>
         
             <div class="row-fluid">

    <c:if test="${not empty exception}">
      <p>
        <p>
        <strong><spring:message code="exception_message" /></strong>
        </p>
        
          <c:out value="${exception.localizedMessage}" />
            <p>
        <strong><spring:message  code="exception_stacktrace" /></strong>
        </p>
          <c:forEach items="${exception.stackTrace}" var="trace">
            <c:out value="${trace}" />
            <br />
          </c:forEach>
      
    </c:if>
    </div>
   </div>
  </div>

</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp" %>