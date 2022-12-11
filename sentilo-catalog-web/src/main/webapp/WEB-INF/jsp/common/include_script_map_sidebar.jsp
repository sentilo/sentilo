<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<div id="map-sidebar-menu-button" style="display: none">
	<button onclick="openMapSidebar()">
		<i class="icon-align-justify"></i>
	</button>
</div>
<div id="map-sidebar">
	<div id="map-sidebar-close-button">
		<a onclick="closeMapSidebar()" class="btn btn-link">
			<i class="icon-remove"></i>
		</a>
	</div>
	<div class="content">
		<h3><spring:message code="sidebar.title" /></h3>
		<hr />
	    <form class="form-search form-inline" onsubmit="return false;">
		    <div class="input-prepend">
			  <span class="add-on"><i class="icon-filter"></i></span>
			  <input id="searchByType" type="text" class="span2" placeholder="<spring:message code="sidebar.search.placeholder" />" data-provide="typeahead" autocomplete="off">
			</div>			    
    	</form>
    	<hr />
   		<div id="tree" class="tree"></div>
	</div>
</div>

<script type="text/javascript">
	//Incomming ct data from request
	var rqct = '<c:out value="${param.ct}" />';
	//Incomming ctc data from request
	var rqctc = '<c:out value="${param.tct}" />';

	// Component types data
	var componentTypes = {
		<c:forEach var="componentType" items="${componentTypes}">
		'${componentType.id}': {
			'id': '${componentType.id}',
			'icon': '${iconsPath}/${componentType.icon}.png',
			'tags': '${componentType.tags}',
			'name': '<spring:eval expression="componentType.name" htmlEscape="false" javaScriptEscape="true" />'
		},
		</c:forEach>
	};

	// Map sidebar constants
	var initialState = 'open';
	
	// Initialize global variables treeConfig and treeData	
	treeConfig = {
		initialFilterIds: rqct,
		initialFilterCategories: rqctc,		
		initialState: initialState,
		translations: {
			checkall: {
				text: '<spring:message code="sidebar.tree.categories.all" />'
			},
			categories: {
				none: '<spring:message code="sidebar.tree.categories.other" />'
			}
		}
	}
		
	treeData = getTreeData(); 
		
	$(document).ready(function () {
		initMapSidebar();
	});
	
</script>