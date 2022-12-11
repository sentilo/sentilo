<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<c:if test="${mode == 'edit' && not empty componentType.icon}">
	<c:set value="${componentType.icon}" var="defaultSelectedIcon" />
</c:if>

<c:if test="${mode == 'create' || empty componentType.icon}">
	<c:set value="pins6" var="defaultSelectedIcon" />
</c:if>

<spring:url value="/static/img/icons" var="iconsPath" />

<script>
var selectComponentTypeIcon = function(which) {
	var nameWithoutExtension = which.replace('.png','');
	$('#selectedIconImage').attr('src', '${iconsPath}/' + which);
	$('#selectedIcon').val(nameWithoutExtension);
}

$(document).ready(function() {
	selectComponentTypeIcon('${defaultSelectedIcon}.png');
});
</script>

<div class="control-group">
	<form:label path="icon" class="control-label">
		<spring:message code="componenttype.icon" />
	</form:label>
	<div class="controls">
		<form:hidden path="icon" id="selectedIcon" />
		<div class="btn-group connecta-icon-left connecta-icon-group">
			<a href="#" data-toggle="dropdown" class="btn dropdown-toggle"> &nbsp;&nbsp;&nbsp;
				<img src="${iconsPath}/pins1.png" id="selectedIconImage"> <span class="caret"></span> 
			</a>
			<ul class="dropdown-menu connecta-icon-dropdown-menu">
			   <c:forEach items="${componentTypeIcons}" var="element"> 
			   	<li><a href="#" onclick="selectComponentTypeIcon('${element}');"><img src="${iconsPath}/${element}"> </a></li>
			   </c:forEach>				
			</ul>
		</div>
	</div>
</div>
