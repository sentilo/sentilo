<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@include file="/WEB-INF/jsp/common/taglibs.jsp"%>

<spring:message code="datatables.sFirst" var="sFirst" />
<spring:message code="datatables.sPrevious" var="sPrevious" />
<spring:message code="datatables.sNext" var="sNext" />
<spring:message code="datatables.sLast" var="sLast" />
<spring:message code="datatables.sInfo" var="sInfo" />
<spring:message code="datatables.sSearch" var="sSearch" />
<spring:message code="datatables.sLengthMenu" var="sLengthMenu" />
<spring:message code="datatables.sEmptyTable" var="sEmptyTable" />

<script type="text/javascript">

var asyncTable;
var detail_prefix = '';
var linkable = true;
var paramsSearchMap;
var firstPass = true;

var tuneUpTable = function() {

	if (asyncTable.fnGetData().length && linkable) {		
		var rows = $(asyncTable.tableSelector + ' tbody tr');
		rows.click(function () {
			var aData = asyncTable.fnGetData(this, 0);			
			document.location.href = detail_prefix + aData + '/detail';
		});
		rows.css('cursor', 'pointer');
	}
	
	// After do search, unchecked selectAllRows input if it is checked
    // and change onclick event for every selectedIds element	
	if($("input[name='selectAllRows']")){
		$("input[name='selectAllRows']").prop('checked', false);
		
		$("input[name='selectedIds']").unbind('click');
		
		var ckbxs = $("input[name='selectedIds']");
		ckbxs.click(function (event) {
			//If one checkbox is unckecked then checkbox selectAllRows is unCheck		
			if (!$(this).is(':checked')){
				$("input[name='selectAllRows']").prop('checked', false);
			}
		});	
	}
}

function reFillSearch(asyncTable){
	if(asyncTable && paramsSearchMap){
		$(asyncTable.tableSelector+'_filter :input').val(paramsSearchMap.wordToSearch);
		$('select[name="' + asyncTable.tableSelector.substring(1)+'_length' + '"]').val(paramsSearchMap.pageSize);
		var oSettings = asyncTable.fnSettings();
		oSettings.oPreviousSearch.sSearch = paramsSearchMap.wordToSearch;
		oSettings._iDisplayLength = parseInt(paramsSearchMap.pageSize);
		oSettings._iDisplayStart = parseInt(paramsSearchMap.pageNumber,10) * paramsSearchMap.pageSize;
		asyncTable.oApi._fnUpdateInfo(oSettings);
	}
	paramsSearchMap = null;
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
    var pageNum = (start == 0) ? 1 : Math.round((start / pageSize) + 1); // pageNum is 1 based
    

    // extract sort information
    var sortCol = paramMap.iSortCol_0;
    var sortDir = paramMap.sSortDir_0;
    var sortName = paramMap['mDataProp_' + sortCol];
    var sEcho = paramMap.sEcho;
    
    var searchTextBack;
    if(firstPass && paramsSearchMap){
    	pageNum = (paramsSearchMap.pageNumber == 0)? 1 : parseInt(paramsSearchMap.pageNumber) + 1;
    	pageSize = paramsSearchMap.pageSize;
    	sortName = paramsSearchMap.sortColumn;
    	sortDir = paramsSearchMap.sortDirecction;
    	if (paramsSearchMap.wordToSearch) {
    		searchTextBack = escape(paramsSearchMap.wordToSearch);
    	}
    	firstPass = false;
    }
    
    var restParams = [
    	{'name' : 'page.page', 'value' : pageNum },
    	{'name' : 'page.size', 'value' : pageSize },
    	{'name' : 'page.sort', 'value' : sortName },
    	{'name' : 'page.sort.dir', 'value' : sortDir },
    	{'name' : 'sEcho', 'value' : sEcho },
    	{'name' : 'tableName', 'value' : this.selector.substring(1) }
    ];
    
    if (paramMap.sSearch) {
    	var searchText = escape(paramMap.sSearch);
        restParams.push({ 'name' : 'search', 'value' : searchText});
    }else if(searchTextBack){
    	restParams.push({ 'name' : 'search', 'value' : searchTextBack});
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
		'sFirst': '${sFirst}',
		'sNext': '${sNext}',
		'sPrevious': '${sPrevious}',
		'sLast': '${sLast}'
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

function  _makeTableAsync(sAjaxSource, selector, detailPrefix, mapSearch, detailLink, firstColumnRenderDelegate,orderable) {
	
	if (typeof detailLink != 'undefined') {
		linkable = detailLink;
	}
	
	detail_prefix = detailPrefix;

	//tableSelector = selector;
	
	if (typeof(firstColumnRenderDelegate) === 'undefined') {
		firstColumnRenderDelegate = function (data, type, row) {
			// Checkbox must be displayed if and only if row.length = table.columns.length
			var dtColumns = this.asyncTable.dataTableSettings[0].aoColumns.length;
			if(row.length > dtColumns && row[dtColumns].indexOf('"hideCheckbox":"true"') > -1){				
				return '';
			}else{
				return '<input type="checkbox" name="selectedIds" value="' + data +'" onclick="dontShowDetail(event);"/>';
			}
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
	
	if(mapSearch){
		paramsSearchMap = mapSearch;
		defaultSorting = [[mapSearch.sortColumn,mapSearch.sortDirecction]]; 
	}
	
	var orderSort = true;
	if (typeof orderable != 'undefined') {
		orderSort = orderable;
	}
	
	
	asyncTable = $(selector).dataTable( {
		'sDom': '<"row-fluid"<"span6"l><"span6"f>r>t<"row-fluid"<"span6"i><"span6"p>>',
		'sPaginationType': 'bootstrap',
		'oLanguage': tableLabels(),
		'aoColumnDefs': aoColumDefs,
		'bAutoWidth': false,
		'bSortClasses': false,
		"bSort": orderSort,
		'aaSorting': defaultSorting,
		'sAjaxSource' : sAjaxSource,
		'bServerSide' : true,
	    'fnServerData' : translateRESTParameters,
	    'pagingType': 'full_numbers'
	} );
	
	asyncTable.tableSelector = selector;
	
	reFillSearch(asyncTable);
	
	return asyncTable;
    
}

function  makeTableAsync(tableNameSelector, sAjaxSource, detailLink, firstColumnRenderDelegate,orderable){
    var mapSearch;
	
	if('${lastSearchParams}'){
		mapSearch = {'wordToSearch':unescape('${lastSearchParams.wordToSearch}'),
					'pageSize':'${lastSearchParams.pageSize}',
					'pageNumber':'${lastSearchParams.pageNumber}',
					'sortColumn':'${lastSearchParams.sortColum}',
					'sortDirecction':'${lastSearchParams.sortDirecction}',
		};
	}
	
	if($("input[name='selectAllRows']")){
		$("input[name='selectAllRows']").on( "click", function() {			
			if ($("input[name='selectedIds']").length > 0) {
				var checkAll = $(this).is(':checked');		
				$("input[name='selectedIds']").prop('checked', checkAll);	
			} else {
				$(this).prop('checked', false);
			}
		});
	}
	
	// If logged in user has role USER then first column must be rendered empty, i.e., no checkbox column must be displayed	
	<security:authorize access="hasRole('ROLE_USER')">	
		firstColumnRenderDelegate = function (data, type, row) {
		  return '';
		};
	</security:authorize>
	
	
	return _makeTableAsync(sAjaxSource, '#'+tableNameSelector, '${detailPrefix}', mapSearch, detailLink, firstColumnRenderDelegate,orderable);
}
</script>

<spring:url value="/static/js/DT_bootstrap.js" var="dtBootstrapJS" />
<script type="text/javascript" src="${dtBootstrapJS}"></script>
