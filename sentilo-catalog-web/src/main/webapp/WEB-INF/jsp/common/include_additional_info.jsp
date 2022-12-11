<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:url value="/static/js/sentilo/additional_info.js" var="additionalInfoJs"/>

<c:choose>
	<c:when test="${mode == 'detail'}">
		<c:set var="additionalInfoTableName" value="detail_aitn" />
	</c:when>	
	<c:otherwise>
		<c:set var="additionalInfoTableName" value="editable_aitn" />
	</c:otherwise>
</c:choose>


<style type="text/css">
	table.ai-table tr:nth-child(even) {
	  background-color: #f2f2f2;
	}
</style>

<div class="accordion-group">
	<div class="accordion-heading">
		<a class="accordion-toggle" data-toggle="collapse" data-parent="#detailAdditionalInfoAccordion" href="#detailAdditionalInfoAccordionCollapse"> 
			<i class="icon-th"></i> <spring:message code="data" /> 
			<i class="icon-chevron-down pull-right"></i> 
		</a>
	</div>
	<div id="detailAdditionalInfoAccordionCollapse" class="accordion-body collapse in">
		<!-- AdditionalInfo is saved in a Map into the resource object, so to print field/value we must iterate over the map -->
		<div class="accordion-inner">							
			<table class="table ai-table table-striped table-responsive-md table-sm" id="${additionalInfoTableName}">
			     <thead>
			      <tr>
			        <th name ="key" class="span4"><spring:message code="additionalInfo.key" /></th>
			        <th name="value" class="span8"><spring:message code="additionalInfo.value" /></th>        
			      </tr>
			    </thead>
			    <tbody>
				<c:if test="${!empty additionalInfo}">																				
					<c:forEach var="entry" items="${additionalInfo}">					  
					  <tr>
				        <td name="key" class="span4 column-additional-info">${entry.key}</td>
				        <td name="value" class="span16 column-additional-info">${entry.value}</td>        
				      </tr>						
					</c:forEach>					
				</c:if>
				</tbody>
			</table>
			<c:if test="${mode != 'detail'}">
				<span style="float:right"><a href="#" id="but_add" class="btn"><spring:message code="additionalInfo.btn.add" /></a></span>				
			</c:if>					
		</div>
	</div>
</div>

<c:if test="${mode != 'detail'}">
	<input type="hidden" name="additionalInfo" id="additionalInfo" value="" />
</c:if>	



<script src="${additionalInfoJs}" type="text/javascript"></script>

