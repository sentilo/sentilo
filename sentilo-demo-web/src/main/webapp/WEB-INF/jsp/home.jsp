<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/header.jsp"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div class="container-fluid">
    <div class="content">
    <div class="row-fluid">

	<%@include file="/WEB-INF/jsp/common/messages.jsp" %>
	<div id="widget" class="span9">
    	<div class="row-fluid">
       
        <div class="span12"> <br>
         <div class="pull-right logo"></div><br><br>   
         <div class=""><h1 class="lead"><spring:message code="home.title.section"/><br><small><spring:message code="home.title.subsection"/></small></h1>
            </div>
        </div> 
         </div>
             <div class="row-fluid">
                <div class="span12">
					<spring:message code="home.text" />
	        	</div>
	    	</div>
		</div>
	</div>

</div>
</div>

<%@include file="/WEB-INF/jsp/common/footer.jsp" %>