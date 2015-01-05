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
var paramsSearchMap;
var firstPass = true;

var tuneUpTable = function() {

	if (asyncTable.fnGetData().length && linkable) {		
		var rows = $(tableSelector + ' tbody tr');
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

function updatePaginationNumber (oSettings){
	// Change the Pagination Number with the new oSetting
	var an = oSettings.aanFeatures.p;
	var oPaging = oSettings.oInstance.fnPagingInfo();
	var iListLength = 5;
	var i, ien, j, sClass, iStart, iEnd, iHalf=Math.floor(iListLength/2);
	
	if ( oPaging.iTotalPages < iListLength) {
		iStart = 1;
		iEnd = oPaging.iTotalPages;
	}
	else if ( oPaging.iPage <= iHalf ) {
		iStart = 1;
		iEnd = iListLength;
	} else if ( oPaging.iPage >= (oPaging.iTotalPages-iHalf) ) {
		iStart = oPaging.iTotalPages - iListLength + 1;
		iEnd = oPaging.iTotalPages;
	} else {
		iStart = oPaging.iPage - iHalf + 1;
		iEnd = iStart + iListLength - 1;
	}
	for ( i=0, ien=an.length ; i<ien ; i++ ) {
			// Remove the middle elements
			$('li:gt(0)', an[i]).filter(':not(:last)').remove();

			// Add the new list items and their event handlers
			for ( j=iStart ; j<=iEnd ; j++ ) {
				sClass = (j==oPaging.iPage+1) ? 'class="active"' : '';
				$('<li '+sClass+'><a href="#">'+j+'</a></li>')
					.insertBefore( $('li:last', an[i])[0] )
					.bind('click', function (e) {
						e.preventDefault();
						oSettings._iDisplayStart = (parseInt($('a', this).text(),10)-1) * oPaging.iLength;
						fnDraw( oSettings );
					} );
			}

			// Add / remove disabled classes from the static elements
			if ( oPaging.iPage === 0 ) {
				$('li:first', an[i]).addClass('disabled');
			} else {
				$('li:first', an[i]).removeClass('disabled');
			}

			if ( oPaging.iPage === oPaging.iTotalPages-1 || oPaging.iTotalPages === 0 ) {
				$('li:last', an[i]).addClass('disabled');
			} else {
				$('li:last', an[i]).removeClass('disabled');
			}
		}
	
}

function reFillSearch(){
	if(asyncTable && paramsSearchMap){
		$(tableSelector+'_filter :input').val(paramsSearchMap.wordToSearch);
		$('select[name="' + tableSelector.substring(1)+'_length' + '"]').val(paramsSearchMap.pageSize);
		var oSettings = asyncTable.fnSettings();
		oSettings.oPreviousSearch.sSearch = paramsSearchMap.wordToSearch;
		oSettings._iDisplayLength = paramsSearchMap.pageSize;
		oSettings._iDisplayStart = parseInt(paramsSearchMap.pageNumber,10) * paramsSearchMap.pageSize;
		asyncTable.oApi._fnUpdateInfo(oSettings);
		
		updatePaginationNumber(oSettings);
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
    	{'name' : 'tableName', 'value' : tableSelector.substring(1) }
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

function  _makeTableAsync(sAjaxSource, selector, detailPrefix, mapSearch, detailLink, firstColumnRenderDelegate,orderable) {
	
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
	
	if(mapSearch){
		paramsSearchMap = mapSearch;
		defaultSorting = [[mapSearch.sortColumn,mapSearch.sortDirecction]]; 
	}
	
	var orderSort = true;
	if (typeof orderable != 'undefined') {
		orderSort = orderable;
	}
	
	
	asyncTable = $(tableSelector).dataTable( {
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
	    'fnServerData' : translateRESTParameters
	} );
	
	reFillSearch();
	
	return asyncTable;
    
}

function  makeTableAsync(tableNameSelector, sAjaxSource, detailLink, firstColumnRenderDelegate,orderable){
    var mapSearch;
	
	if('${lastSearchParams}'){
		mapSearch = {'wordToSearch':'${lastSearchParams.wordToSearch}',
					'pageSize':'${lastSearchParams.pageSize}',
					'pageNumber':'${lastSearchParams.pageNumber}',
					'sortColumn':'${lastSearchParams.sortColum}',
					'sortDirecction':'${lastSearchParams.sortDirecction}',
		};
	}
	
	if($("input[name='selectAllRows']")){
		$("input[name='selectAllRows']").on( "click", function(){
			var checkAll = $(this).is(':checked');		
			$("input[name='selectedIds']").prop('checked', checkAll);				
		});
	}
	
	return _makeTableAsync(sAjaxSource, '#'+tableNameSelector, '${detailPrefix}', mapSearch, detailLink, firstColumnRenderDelegate,orderable);
}
</script>

<spring:url value="/static/js/DT_bootstrap.js" var="dtBootstrapJS" />
<script type="text/javascript" src="${dtBootstrapJS}"></script>
