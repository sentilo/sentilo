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

var firstPass = true;

var tuneUpTable = function(asyncTable) {

	if (asyncTable.fnGetData().length && asyncTable.linkable) {		
		var rows = $(asyncTable.tableSelector + ' tbody tr');
		rows.click(function () {
			var aData = asyncTable.fnGetData(this, 0);			
			document.location.href = asyncTable.detail_prefix + aData + '/detail';
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
	
	if(asyncTable && asyncTable.paramsSearchMap){
		$(asyncTable.tableSelector+'_filter :input').val(asyncTable.paramsSearchMap.wordToSearch);
		$('select[name="' + asyncTable.tableSelector.substring(1)+'_length' + '"]').val(asyncTable.paramsSearchMap.pageSize);
		var oSettings = asyncTable.fnSettings();
		oSettings.oPreviousSearch.sSearch = asyncTable.paramsSearchMap.wordToSearch;
		oSettings._iDisplayLength = parseInt(asyncTable.paramsSearchMap.pageSize);
		oSettings._iDisplayStart = parseInt(asyncTable.paramsSearchMap.pageNumber,10) * asyncTable.paramsSearchMap.pageSize;
		asyncTable.oApi._fnUpdateInfo(oSettings);
		
		asyncTable.paramsSearchMap = null;
	}	
}

var translateRESTParameters = function(url, aoData, fnCallback, oSettings) {
	
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
    var asyncTable = oSettings.oInstance;
    var paramsSearchMap = oSettings.paramsSearchMap;
    
    if(!asyncTable.paramsSearchMap && oSettings.oInit.paramsSearchMap){
    	asyncTable.paramsSearchMap = oSettings.oInit.paramsSearchMap
    }
    
    if(firstPass && asyncTable.paramsSearchMap){
    	pageNum = (asyncTable.paramsSearchMap.pageNumber == 0)? 1 : parseInt(asyncTable.paramsSearchMap.pageNumber) + 1;
    	pageSize = asyncTable.paramsSearchMap.pageSize;
    	sortName = asyncTable.paramsSearchMap.sortColumn;
    	sortDir = asyncTable.paramsSearchMap.sortDirecction;
    	if (asyncTable.paramsSearchMap.wordToSearch) {
    		searchTextBack = escape(asyncTable.paramsSearchMap.wordToSearch);
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
		tuneUpTable(asyncTable);                   
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
	
	var defaultSorting = mapSearch ? [[mapSearch.sortColumn,mapSearch.sortDirecction]] : [[1, "asc"]];		
	var orderSort = typeof orderable != 'undefined' ? orderable : true;
		
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
	    'pagingType': 'full_numbers',
	    'paramsSearchMap': mapSearch 
	} );
	
	// Set additional attributes to asyncTable instance			
	asyncTable.tableSelector = selector;		
	
	asyncTable.detail_prefix = detailPrefix;
	asyncTable.linkable = typeof detailLink != 'undefined' ? detailLink : true;
	asyncTable.paramsSearchMap = mapSearch;
	
	reFillSearch(asyncTable);
	makeTableScrollable(selector);
	
	return asyncTable;
    
}

function makeTableScrollable(selector) {
	// Wrapp data table & allow to overflow with x-scroll
	$(selector).wrap('<div class="dataTable-overfow"></div>');
	$('form[id="command"]').wrap('<div class="dataTable-overfow-wrapper"></div>');
}

/**
 * Defines a new object dataTable where its data is loaded asynchronously with an AJAX request.
 * @param tableNameSelector: unique identifier for the list. Must match with the id of a DOM element (mandatory)  
 * @param sAjaxSource: AJAX request to get data list (mandatory)
 * @param detailPrefixURL: URL prefix to access to the detail of each row. If it's empty or undefined then rows are not linkable (optional)
 * @param firstColumnRenderDelegate: function to render the first column of each row. If it's not defined, a checkbox is rendered (optional)
 * @param orderable: set if the list is orderable. By default, lists are orderable (optional)
 */
function  makeTableAsync(tableNameSelector, sAjaxSource, detailPrefixURL, firstColumnRenderDelegate, orderable){
    
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
		
	var linkable = detailPrefixURL? true: false;
			
	return _makeTableAsync(sAjaxSource, '#'+tableNameSelector, detailPrefixURL, mapSearch, linkable, firstColumnRenderDelegate, orderable);
}
</script>

<spring:url value="/static/js/DT_bootstrap.js" var="dtBootstrapJS" />
<script type="text/javascript" src="${dtBootstrapJS}"></script>
