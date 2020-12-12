<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/static/img/icons" var="iconsPath" />

<script type="text/javascript">
function selectComponentType(id, name, icon) {
	$('#selectedComponentType').val(id);
	$('#selectedComponentIcon').attr('src', '${iconsPath}/' + icon + '.png');
	$('#selectedComponentLabel').html(name);
	smap_init(true);
}
</script>

<div id="map_controls" class="hide">
	<form class="form-inline row-fluid">
		<spring:message code="component.map.component.type.all" var="allTypesLabel" />

		<input type="hidden" id="selectedComponentType" name="selectedComponentType" value="" />
		<div class="btn-group connecta-icon-group">
			<a href="#" data-toggle="dropdown" class="btn dropdown-toggle"> &nbsp;&nbsp;&nbsp; <img
				src="${iconsPath}/pins6.png" id="selectedComponentIcon" /> <span id="selectedComponentLabel">${allTypesLabel}</span>
				<span class="caret"></span> </a>
			<ul class="dropdown-menu pull-right" id="componentDropdown">
				<c:forEach var="componentType" items="${componentTypes}">
					<li id='${componentType.id}'><a href="#"
						onclick="selectComponentType('${componentType.id}', '<spring:eval expression="componentType.name" htmlEscape="false" javaScriptEscape="true" />', '${componentType.icon}')">
						<img src="${iconsPath}/${componentType.icon}.png">${componentType.name}</a>
					</li>
				</c:forEach>
				<li class="divider"></li>
				<li><a href="#" onclick="selectComponentType('', '${allTypesLabel}', 'pins6')"><img	src="${iconsPath}/pins6.png">${allTypesLabel}</a></li>
			</ul>
		</div>

		<div class="input-prepend input-append" id="geocode">
			<span class="add-on"> <i class="icon-map-marker"></i> </span>
			<spring:message code="component.map.address.search.instructions" var="instructions" />
			<input id="address" class="" placeholder="${instructions}" type="text" data-provide="typeahead" autocomplete="off" />
			<button id="search" class="btn" type="button">
				<spring:message code="component.map.address.search.action" />
			</button>
		</div>
		<button id="locate" class="btn" type="button">
			<i class="connecta-icon-location">&nbsp;&nbsp;&nbsp;</i>
		</button>
	</form>
</div>
<div id="map_canvas_1" class="${mapClass}"></div>
