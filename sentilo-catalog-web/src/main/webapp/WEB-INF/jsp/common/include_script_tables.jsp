<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="datatables.sPrevious" var="sPrevious" />
<spring:message code="datatables.sNext" var="sNext" />
<spring:message code="datatables.sInfo" var="sInfo" />
<spring:message code="datatables.sSearch" var="sSearch" />
<spring:message code="datatables.sLengthMenu" var="sLengthMenu" />
<spring:message code="datatables.sEmptyTable" var="sEmptyTable" />

<script type="text/javascript">

var tableSelector = '';
var asyncTable;
var detail_prefix = '';
var linkable = true;

var tuneUpTable = function() {

	if (asyncTable.fnGetData().length && linkable) {
		
		var rows = $(tableSelector + ' tbody tr');
		rows.click(function () {
			var aData = asyncTable.fnGetData(this, 0);
			document.location.href = detail_prefix + aData + '/detail';
		});
		rows.css('cursor', 'pointer');
	}
}

var translateRESTParameters = function(url, aoData, fnCallback) {

    //extract name/value pairs into a simpler map for use later
    var paramMap = {};
    for ( var i = 0; i < aoData.length; i++) {
        paramMap[aoData[i].name] = aoData[i].value;
    }
    
    //page calculations
    var pageSize = paramMap.iDisplayLength;
    var start = paramMap.iDisplayStart;
    var pageNum = (start == 0) ? 1 : (start / pageSize) + 1; // pageNum is 1 based

    // extract sort information
    var sortCol = paramMap.iSortCol_0;
    var sortDir = paramMap.sSortDir_0;
    var sortName = paramMap['mDataProp_' + sortCol];
    var sEcho = paramMap.sEcho;
    
    var restParams = [
    	{'name' : 'page.page', 'value' : pageNum },
    	{'name' : 'page.size', 'value' : pageSize },
    	{'name' : 'page.sort', 'value' : sortName },
    	{'name' : 'page.sort.dir', 'value' : sortDir },
    	{'name' : 'sEcho', 'value' : sEcho }
    ];
    
    if (paramMap.sSearch) {
    	var searchText = escape(paramMap.sSearch);
        restParams.push({ 'name' : 'search', 'value' : searchText});
    }
    
    jsonGET(url, restParams, function(data) {
        data.iTotalRecords = data.totalCount;
        data.iTotalDisplayRecords = data.totalCount;
        fnCallback(data);
		tuneUpTable();            
    });
};

function tableLabels() {
	var paginationLabels = {
		'sNext': '${sNext}',
		'sPrevious': '${sPrevious}',
	}
	var oLanguage = {
		'sLengthMenu': '${sLengthMenu}',
		'sInfo': '${sInfo}',
		'sInfoEmpty': '',
		'sSearch': '${sSearch}',
		'sEmptyTable': '${sEmptyTable}',
		'oPaginate': paginationLabels
	}
	return oLanguage;
}

function dontShowDetail(event) {
	event.stopPropagation();
}

function makeTableAsync(sAjaxSource, selector, detailPrefix, detailLink, firstColumnRenderDelegate) {
	
	if (typeof detailLink != 'undefined') {
		linkable = detailLink;
	}
	
	detail_prefix = detailPrefix;

	tableSelector = selector;
	
	if (typeof(firstColumnRenderDelegate) === 'undefined') {
		firstColumnRenderDelegate = function (data, type, row) {
			return '<input type="checkbox" name="selectedIds" value="' + data +'" onclick="dontShowDetail(event);"/>';
		}; 
	}
	
	var firstColumnDefinition = {
		'bSortable': false,
		'aTargets': [0],
		'mRender': firstColumnRenderDelegate
	}
	
	var aoColumDefs = [ 
		firstColumnDefinition
	];
	
	var defaultSorting = [[1, "asc"]];
	
	asyncTable = $(tableSelector).dataTable( {
		'sDom': '<"row-fluid"<"span6"l><"span6"f>r>t<"row-fluid"<"span6"i><"span6"p>>',
		'sPaginationType': 'bootstrap',
		'oLanguage': tableLabels(),
		'aoColumnDefs': aoColumDefs,
		'bAutoWidth': false,
		'bSortClasses': false,
		'aaSorting': defaultSorting,
		'sAjaxSource' : sAjaxSource,
		'bServerSide' : true,
	    'fnServerData' : translateRESTParameters
	} );
	
	$('.dataTables_filter input')
		.unbind('keypress keyup')
		.bind('keyup', function(e) {
			if (e.keyCode != 13) {
				return;
			}
			asyncTable.fnFilter($(this).val());
	    });
}

</script>


<spring:url value="/static/js/DT_bootstrap.js" var="dtBootstrapJS" />
<script type="text/javascript" src="${dtBootstrapJS}"></script>


