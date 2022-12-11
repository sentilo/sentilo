<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp" %>
<spring:eval expression="@sentiloConfigProperties.getProperty('sentilo.catalog.ribbon.text')" var="ribbonText" />

    <footer>
        <div class="row-fluid">
            <div class="span6">
                <spring:message code="footer.copy" htmlEscape="false"/>
                <br>
            </div>
        </div>
    </footer>
    <%@include file="/WEB-INF/jsp/common/footer-scripts.jsp" %>
    <c:if test="${not empty ribbonText}">
	    <div class="ribbon">
	    	<span>${ribbonText}</span>
	    </div>
    </c:if>
    </body>
</html>
