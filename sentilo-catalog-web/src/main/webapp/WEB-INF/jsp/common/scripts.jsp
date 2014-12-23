<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="sure.delete" var="deleteConfirmMessage" />
<spring:message code="sure.unassign" var="unassignConfirmMessage" />
<spring:message code="select.one" var="selectOneErrorMessage" />
<spring:message code="ok" var="okButtonLabel" />

<script type="text/javascript">
function openDetail(url) {
	window.location.href = url;
};

function deleteSelected(formName, deleteConfirmMessage) {
	if(deleteConfirmMessage){
		confirmFormSubmission(formName, deleteConfirmMessage);
	}else{
		confirmFormSubmission(formName, '${deleteConfirmMessage}');
	}
};


function changeAccessType(formName, confirmMessage, newType, changeAccessTypeUrl){		
	var accessTypeHiddenField = '<input type="hidden" name="newAccessType" value="'+newType+'" />';
	confirmFormSubmission(formName, confirmMessage, accessTypeHiddenField, changeAccessTypeUrl);
}

function unassignSelected(formName) {
	confirmFormSubmission(formName, '${unassignConfirmMessage}');	
};

function confirmFormSubmission(formName, message, childHiddenField, formActionUrl) {
	var n = $('input:checked').length;
	if (n > 0) {
		bootbox.confirm(message, function(result) {
			if(result == true) {
				if(childHiddenField){
					$('form#' + formName).append(childHiddenField);
				}
				
				if(formActionUrl){
					$('form#' + formName).attr('action',formActionUrl);
				}
				$('form#' + formName).submit();
			}
		});
	} else {
		bootbox.dialog('${selectOneErrorMessage}', [{
			'label' : '${okButtonLabel}',
			'class' : 'btn-danger'
		}]);
	}
};

function preventEnterSubmit(event) {
	return event.keyCode != 13;
};

function addTimestampToURL(url) {
	var ts = new Date().getTime();
	if (url.indexOf('?') !== -1) {
		return url + '&ts=' + ts;
	}
	return url + '?ts=' + ts;
}

function addParamToUrl(url, paramName, paramValue){
	var charToAdd = (url.indexOf('?') !== -1 ? '&' :'?');	
	return (paramValue && paramValue!='' ? url + charToAdd +paramName+'=' + paramValue : url);
}

function jsonGET(url, data, success) {
    $.ajax({
    	'contentType':'application/x-www-form-urlencoded; charset=UTF-8',
        'dataType' : 'json',
        'type' : 'GET',
        'url' : addTimestampToURL(url),
        'data' : data,
        'success' : success
    });
};

function formatGraphTimestamp(timestamp) {
	return timestamp.replace('T', '<br/>');
};

function addLeadingZeroes(text) {
	return ('0' + text).slice(-2);
}  

function formatTimestamp(timestamp) {
	return timestamp.replace('T', ' ');
};

function isValidDecimalNumber(number) {
	 return (/^([0-9])*[.]?[0-9]*$/.test(number));
}

</script>

<spring:url value="/static/js/jquery-1.9.0.js" var="jqueryJS" />
<spring:url value="/static/js/bootstrap.min.js" var="bootstrapJS" />
<spring:url value="/static/js/jquery.dataTables.js" var="jqueryDataTablesJS" />
<spring:url value="/static/js/bootbox.min.js" var="bootboxJS" />
<spring:url value="/static/js/jquery.pnotify.min.js" var="jqueryPinesNotifyJS" />
<spring:url value="/static/js/jquery.tagsinput.js" var="jqueryTagsInputJS" />

<script type="text/javascript" src="${jqueryJS}"></script>
<script type="text/javascript" src="${bootstrapJS}"></script>
<script type="text/javascript" src="${bootboxJS}"></script>
<script type="text/javascript" src="${jqueryDataTablesJS}"></script>
<script type="text/javascript" src="${jqueryPinesNotifyJS}"></script>
<script type="text/javascript" src="${jqueryTagsInputJS}"></script>

<%@include file="/WEB-INF/jsp/common/customScripts.jsp"%>