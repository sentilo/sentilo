<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<%@ include file="/WEB-INF/jsp/common/include_common_messages.jsp"%>


<script type="text/javascript">

function openDetail(url) {
	window.location.href = url;
};

function deleteSelected(formName, customDeleteConfirmMessage) {
	if(customDeleteConfirmMessage){
		confirmFormSubmission(formName, customDeleteConfirmMessage);
	}else{
		confirmFormSubmission(formName, deleteConfirmMsg);
	}
};

function changeAccessType(formName, confirmMessage, newType, changeAccessTypeUrl){		
	var accessTypeHiddenField =  buildTextHiddenField("newAccessType",newType );
	var hiddenFields = [accessTypeHiddenField];
	confirmFormSubmission(formName, confirmMessage, changeAccessTypeUrl, hiddenFields);
}

function changeMapVisibility(formName, confirmMessage, newMapVisibility, changeMapVisibilityUrl){		
	var changeMapVisibilityHiddenField = buildTextHiddenField("newMapVisibility",newMapVisibility ); 
	var hiddenFields = [changeMapVisibilityHiddenField];
	confirmFormSubmission(formName, confirmMessage, changeMapVisibilityUrl, hiddenFields);
}

function changeListVisibility(formName, confirmMessage, newListVisibility, changeListVisibilityUrl){		
	var changeListVisibilityHiddenField = buildTextHiddenField("newListVisibility",newListVisibility ); 
	var hiddenFields = [changeListVisibilityHiddenField];
	confirmFormSubmission(formName, confirmMessage, changeListVisibilityUrl, hiddenFields);
}

function changeSensorsState(formName, confirmMessage, changeStateUrl){		
	var newSensorsState = $("#sensorState").val();
	var newSensorsSubstate = $("#sensorSubstate").val();
	var sensorsStateHiddenField =  buildTextHiddenField("newState",newSensorsState );
	var sensorsSubstateHiddenField =  buildTextHiddenField("newSubstate",newSensorsSubstate ); 
	
	var hiddenFields = [sensorsStateHiddenField] ;	
	hiddenFields.push(sensorsSubstateHiddenField);
	
	
	confirmFormSubmission(formName, confirmMessage, changeStateUrl, hiddenFields);
}

function unassignSelected(formName) {
	confirmFormSubmission(formName, unassignConfirmMsg);	
};

function confirmFormSubmission(formName, message, formActionUrl, childrenHiddenFields) {
	var n = $('input:checked').length;
	if (n > 0) {
		bootbox.confirm(message, function(result) {
			if(result == true) {
				if(childrenHiddenFields && childrenHiddenFields.length > 0){
					for (i = 0; i < childrenHiddenFields.length; i++) {
						$('form#' + formName).append(childrenHiddenFields[i]);
					} 										
				}
				
				if(formActionUrl){
					$('form#' + formName).attr('action',formActionUrl);
				}
				$('form#' + formName).submit();
			}
		});
	} else {
		bootbox.dialog(selectOneErrorMsg, [{
			'label' : okButtonLabelMsg,
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

function addDefaultHeaders() {
    var token = $("meta[name='_csrf']").attr("content");
    var header = $("meta[name='_csrf_header']").attr("content");
    $(document).ajaxSend(function(e, xhr, options) {
        xhr.setRequestHeader(header, token);
    });
};

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

function jsonPOST(url, data, success) {
    addDefaultHeaders();
    $.ajax({
        'contentType':'application/json; charset=UTF-8',
        'dataType' : 'json',
        'type' : 'POST',
        'url' : addTimestampToURL(url),
        'data' : data,
        'success' : success
    });
};


function buildTextHiddenField(fieldName, fieldValue){
	return '<input type="hidden" name="'+fieldName+'" value="'+fieldValue+'" />';	
}

function formatGraphTimestamp(timestamp) {		
	return timestamp.replace(' ','<br/>');
};

function addLeadingZeroes(text) {
	return ('0' + text).slice(-2);
}  

function isValidDecimalNumber(number) {
	 return (/^([0-9])*[.]?[0-9]*$/.test(number));
}

function parseToInteger(value){
	return isFinite(value) ? parseInt(value) : null;
}

function parseToFloat(value){
	return isFinite(value) ? parseFloat(value) : null;
}

function validateJson(json) {
	try {
        JSON.parse(json);
    } catch (e) {
        return false;
    }
    return true;
}

function parseJSON( data ) {
    if ( !data || typeof data !== "string") {
        return null;
    }

    // Make sure leading/trailing whitespace is removed (IE can't handle it)
    data = jQuery.trim( data );

    // Attempt to parse using the native JSON parser first
    if ( window.JSON && window.JSON.parse ) {
        return window.JSON.parse( data );
    }

    // Make sure the incoming data is actual JSON
    // Logic borrowed from http://json.org/json2.js
    if ( rvalidchars.test( data.replace( rvalidescape, "@" )
        .replace( rvalidtokens, "]" )
        .replace( rvalidbraces, "")) ) {

        return ( new Function( "return " + data ) )(); // Just returns JSON data.

    }
    jQuery.error( "Invalid JSON: " + data );
}

var jsonPrettyPrint = {
   replacer: function(match, pIndent, pKey, pVal, pEnd) {
      var key = '<span class=json-key>';
      var val = '<span class=json-value>';
      var str = '<span class=json-string>';
      var r = pIndent || '';
      if (pKey)
         r = r + key + pKey.replace(/[": ]/g, '') + '</span>: ';
      if (pVal)
         r = r + (pVal[0] == '"' ? str : val) + pVal + '</span>';
      return r + (pEnd || '');
      },
   toHtml: function(obj) {
      var jsonLine = /^( *)("[\w]+": )?("[^"]*"|[\w.+-]*)?([,[{])?$/mg;
      return JSON.stringify(obj, null, 3)
         .replace(/&/g, '&amp;').replace(/\\"/g, '&quot;')
         .replace(/</g, '&lt;').replace(/>/g, '&gt;')
         .replace(jsonLine, jsonPrettyPrint.replacer);
      }
   };

</script>

<spring:url value="/static/js/jquery-1.9.0.js" var="jqueryJS" />
<spring:url value="/static/js/bootstrap.min.js" var="bootstrapJS" />
<spring:url value="/static/js/jquery.dataTables.js" var="jqueryDataTablesJS" />
<spring:url value="/static/js/bootbox.min.js" var="bootboxJS" />
<spring:url value="/static/js/jquery.pnotify.min.js" var="jqueryPinesNotifyJS" />
<spring:url value="/static/js/jquery.tagsinput.js" var="jqueryTagsInputJS" />
<spring:url value="/static/js/bootstrap-colorpicker.min.js" var="bootstrapColorPickerJS" />
<spring:url value="/static/js/jquery.jsonPresenter.js" var="jsonPresenterJS" />
<spring:url value="/static/js/sentilo/tooltips.js" var="tooltipsJS" />
<spring:url value="/static/js/wavesurfer.min.js" var="waveSurferJS" />
<spring:url value="/static/js/chartist-plugin-legend.js" var="chartistPluginLegendJS" />
<spring:url value="/static/js/chartist-plugin-tooltip.js" var="chartistPluginTooltipJS" />
<spring:url value="/static/js/chartist.min.js" var="chartistJS" />
<spring:url value="/static/js/sentilo/chartist.js" var="sentiloChartistJS" />
<spring:url value="/static/js/sentilo/media_players.js" var="mediaPlayersJS" />
<spring:eval expression="@catalogConfigProperties.getProperty('catalog.map.provider')" var="provider_map" />

<script type="text/javascript" src="${jqueryJS}"></script>
<script type="text/javascript" src="${bootstrapJS}"></script>
<script type="text/javascript" src="${bootboxJS}"></script>
<script type="text/javascript" src="${jqueryDataTablesJS}"></script>
<script type="text/javascript" src="${jqueryPinesNotifyJS}"></script>
<script type="text/javascript" src="${jqueryTagsInputJS}"></script>
<script type="text/javascript" src="${bootstrapColorPickerJS}"></script>
<script type="text/javascript" src="${jsonPresenterJS}"></script>
<script type="text/javascript" src="${tooltipsJS}"></script>
<script type="text/javascript" src="${waveSurferJS}"></script>
<script type="text/javascript" src="${chartistJS}"></script>
<script type="text/javascript" src="${chartistPluginLegendJS}"></script>
<script type="text/javascript" src="${chartistPluginTooltipJS}"></script>
<script type="text/javascript" src="${sentiloChartistJS}"></script>
<script type="text/javascript" src="${mediaPlayersJS}"></script>


<c:if test="${not empty currentRequestMapping}">
	<c:choose>
		<c:when test="${fn:contains(currentRequestMapping, '/component/map')}">
			<spring:url value="/static/js/sentilo/universal_map.js" var="universalMapJS" />
			<script type="text/javascript" src="${universalMapJS}"></script>
		</c:when>
	</c:choose>
</c:if>

<%@include file="/WEB-INF/jsp/common/customScripts.jsp"%>
