<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="sure.delete" var="deleteConfirmMessage" />
<spring:message code="sure.unassign" var="unassignConfirmMessage" />
<spring:message code="select.one" var="selectOneErrorMessage" />
<spring:message code="ok" var="okButtonLabel" />

<spring:url value="/" var="homeURL" />

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

function unassignSelected(formName) {
	confirmFormSubmission(formName, '${unassignConfirmMessage}');	
};

function confirmFormSubmission(formName, message) {
	var n = $('input:checked').length;
	if (n > 0) {
		bootbox.confirm(message, function(result) {
			if(result == true) {
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
	/* if (timestamp) {
		var date = new Date(timestamp);
		return addLeadingZeroes(date.getDay()) + '/' +
	       addLeadingZeroes((date.getMonth() + 1)) + '/' +
	       addLeadingZeroes(date.getFullYear()) + '<br/>' +
	       addLeadingZeroes(date.getHours()) + ':' +
	       addLeadingZeroes(date.getMinutes()) + ':' + 
	       addLeadingZeroes(date.getSeconds());
	} */
	return timestamp.replace('T', '<br/>');
};

function addLeadingZeroes(text) {
	return ('0' + text).slice(-2);
}  

function formatTimestamp(timestamp) {
	/* if (timestamp) {
		var date = new Date(timestamp);
		return addLeadingZeroes(date.getDay()) + '/' +
		       addLeadingZeroes((date.getMonth() + 1)) + '/' +
		       addLeadingZeroes(date.getFullYear()) + ' ' +
		       addLeadingZeroes(date.getHours()) + ':' +
		       addLeadingZeroes(date.getMinutes()) + ':' + 
		       addLeadingZeroes(date.getSeconds());
	} */
	return timestamp.replace('T', ' ');
};





function isValidDecimalNumber(number) {
	 return (/^([0-9])*[.]?[0-9]*$/.test(number));
}

function changeLocale(locale) {

        /* 
        
          Let's do a background request via ajax/jquery, so:

          1) We avoid to get the "?locale=xx" variable in the browser's URL, that's so inelegant...
          2) We avoid some GET method problems in pages that allow only POST method. If an user changes the language in such pages, his browser will ask to reload the page.
        
        */

        $.ajax({
                'type' : 'GET',
                'url' : '${homeURL}?locale=' + locale,
                'async' : false,
                'cache' : false        
        });

        /* 
        
          Locale setting has been changed, so we can simply reload our page.

        */

        window.location.reload(true);
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
