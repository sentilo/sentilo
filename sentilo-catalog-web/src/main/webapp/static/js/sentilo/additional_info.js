/*
 This script contains code from Bootstable script https://github.com/joel-okello/bootstable
 and from Table2Json https://github.com/Praveent696/Table2Json 
 
 Source has been modified to work with bootstrap 2 and to edit and transform additional info table
*/

  "use strict";
  //Global variables
  var params = null;  		//Parameters
  var colsEdi =null;
  var newColHtml = '<div class="btn-group pull-left">'+
'<button id="bEdit" type="button" class="btn btn-sm btn-default"  onclick="rowEdit(this);">' +
'<i class="icon-pencil"></i>'+
'</button>'+
'<button id="bElim" type="button" class="btn btn-sm btn-default"  onclick="rowElim(this);">' +
'<i class="icon-trash" aria-hidden="true"></i>'+
'</button>'+
'<button id="bAcep" type="button" class="btn btn-sm btn-default"  style="display:none;" onclick="rowAcep(this);">' + 
'<i class="icon-ok"></i>'+
'</button>'+
'<button id="bCanc" type="button" class="btn btn-sm btn-default" style="display:none;"  onclick="rowCancel(this);">' + 
'<i class="icon-remove" aria-hidden="true"></i>'+
'</button>'+
    '</div>';

     var saveColHtml = '<div class="btn-group pull-left">'+
'<button id="bEdit" type="button" class="btn btn-sm btn-default" style="display:none;" onclick="rowEdit(this);">' +
'<i class="icon-pencil"></i>'+
'</button>'+
'<button id="bElim" type="button" class="btn btn-sm btn-default" style="display:none;" onclick="rowElim(this);">' +
'<i class="icon-trash" aria-hidden="true"></i>'+
'</button>'+
'<button id="bAcep" type="button" class="btn btn-sm btn-default"   onclick="rowAcep(this);">' + 
'<i class="icon-ok"></i>'+
'</button>'+
'<button id="bCanc" type="button" class="btn btn-sm btn-default"  onclick="rowCancel(this);">' + 
'<i class="icon-remove" aria-hidden="true"></i>'+
'</button>'+
    '</div>';

var colEdicHtml = '<td name="buttons" class="span2">'+newColHtml+'</td>'; 
var colSaveHtml = '<td name="buttons" class="span2">'+saveColHtml+'</td>';
         
     $.fn.setTableEditable = function (options) {
         var defaults = {
             buttonsColumn:'first',   // Default position of buttons column   		 
             columnsEd: null,         //Index to editable columns. If null all td editables. Ex.: "1,2,3,4,5"
             $addButton: null,        //Jquery object of "Add" button
             onEdit: function() {},   //Called after edition
     		 onBeforeDelete: function() {}, //Called before deletion
             onDelete: function() {}, //Called after deletion
             onAdd: function() {} //Called when added a new row: show new row in edit mode
         };
         
         params = $.extend(defaults, options);
         if(params.buttonsColumn == 'first'){
        	 this.find('thead tr').prepend('<th name="buttons" class="span2"></th>');  //encabezado vacío
        	 this.find('tbody tr').prepend(colEdicHtml);
         }else{
        	 this.find('thead tr').append('<th name="buttons" class="span2"></th>');  //encabezado vacío
        	 this.find('tbody tr').append(colEdicHtml);
         }
         
     	 var $tabedi = this;   //Read reference to the current table, to resolve "this" here.
         //Process "addButton" parameter
         if (params.$addButton != null) {
             params.$addButton.click(function() {
                 rowAddNew($tabedi.attr("id"));                 
             });
         }
         
         if (params.columnsEd != null) {         
             colsEdi = params.columnsEd.split(',');
         }
     };
       
     function iterateEditableFields($cols, tarea) {
         var n = 0;
         $cols.each(function() {
             n++;
             if ($(this).attr('name')=='buttons') return;  //excluye columna de botones
             if (!isEditable(n-1)) return; 
             tarea($(this));
         });
         
         function isEditable(idx) {
             if (colsEdi==null) {  
                 return true;  //todas son editable
             } else {  
                 for (var i = 0; i < colsEdi.length; i++) {
                   if (idx == colsEdi[i]) return true;
                 }
                 return false;  //no se encontró
             }
         }
     }
     
     function setNormalMode(but) {
         $(but).parent().find('#bAcep').hide();
         $(but).parent().find('#bCanc').hide();
         $(but).parent().find('#bEdit').show();
         $(but).parent().find('#bElim').show();
         var $row = $(but).parents('tr');  //accede a la fila
         $row.attr('id', '');  //quita marca
     }
     
     function setEditMode(but) {
         $(but).parent().find('#bAcep').show();
         $(but).parent().find('#bCanc').show();
         $(but).parent().find('#bEdit').hide();
         $(but).parent().find('#bElim').hide();
         var $row = $(but).parents('tr');  //accede a la fila
         $row.attr('id', 'editing');  //indica que está en edición
     }
     function isEditMode($row) {
         if ($row.attr('id')=='editing') {
             return true;
         } else {
             return false;
         }
     }
     function rowAcep(but) {
         //Acepta los cambios de la edición                  
         var $row = $(but).parents('tr');  //accede a la fila
         var $cols = $row.find('td');  //lee campos
         if (!isEditMode($row)) return;  //Ya está en edición
         //Está en edición. Hay que finalizar la edición
         iterateEditableFields($cols, function($td) {  //itera por la columnas
           var cont = $td.find('input').val(); //lee contenido del input
           $td.html(cont);  //fija contenido y elimina controles
         });
         setNormalMode(but);
         params.onEdit($row);
     }
     
     function rowCancel(but) {
     //Rechaza los cambios de la edición
         var $row = $(but).parents('tr');  //accede a la fila
         var $cols = $row.find('td');  //lee campos
         if (!isEditMode($row)) return;  //Ya está en edición
         //Está en edición. Hay que finalizar la edición
         iterateEditableFields($cols, function($td) {  //itera por la columnas
             var cont = $td.find('div').html(); //lee contenido del div
             $td.html(cont);  //fija contenido y elimina controles
         });
         setNormalMode(but);
     }
     function rowEdit(but) {  
         var $td = $("tr[id='editing'] td");
         rowAcep($td)
         var $row = $(but).parents('tr');  
         var $cols = $row.find('td');  
         if (isEditMode($row)) return;  
         iterateEditableFields($cols, function($td) { 
             var cont = $td.html(); 
             var div = '<div style="display: none;">' + cont + '</div>';
             var input = '<input class="form-control input-sm"  value="' + cont + '">';
             $td.html(div + input);  
         });
         setEditMode(but);
     }
     function rowElim(but) {  
         var $row = $(but).parents('tr');  
         params.onBeforeDelete($row);
         $row.remove();
         params.onDelete();
     }
     
     function rowAddNew(tabId) {
    	 var $tab_en_edic = $("#" + tabId);  //Table to edit
         var $filas = $tab_en_edic.find('tbody tr');
         
         if ($filas.length==0) {                          
     		 let $row = $tab_en_edic.find('thead tr');  
             let $cols = $row.find('th');
             let htmlDat = '';
             $cols.each(function() {
                 if ($(this).attr('name')=='buttons') {
                     htmlDat = appendCode(params, colSaveHtml, htmlDat);                     	   
                 } else {                	 
                     //htmlDat = htmlDat+'<td name="'+ $(this).attr('name') +'"></td>';
                	 var td_init = '<td name="'+ $(this).attr('name') +'">';
                	 var div = '<div style="display: none;"></div>';
                     var input = '<input class="form-control input-sm"  value="">';
                     var td_end = '</td>';
                     htmlDat = htmlDat + td_init+div+input+td_end;
                 }
             });  
             
             $tab_en_edic.find('tbody').prepend('<tr>'+htmlDat+'</tr>');
             
         } else {
             let $ultFila = $tab_en_edic.find('tr:last');
             $ultFila.clone().appendTo($ultFila.parent()); 
             //$tab_en_edic.find('tr:last').attr('id','editing'); 
             $ultFila = $tab_en_edic.find('tr:last');
             let $cols = $ultFila.find('td');
             
             $cols.each(function() {                 
                 if ($(this).attr('name')!='buttons') {
                     var div = '<div style="display: none;"></div>';
                     var input = '<input class="form-control input-sm"  value="">';

                     $(this).html(div + input);
                 }
             });		
             $ultFila.find('td:first').html(saveColHtml);
        }
        $tab_en_edic.find('tr:last').attr('id','editing'); 
     	params.onAdd();
     }
     
     function appendCode(params, source1, source2){
    	 return params.buttonsColumn == 'first'? source1 + source2 : source2 + source1;
     }
     
     /*
      * Source from https://github.com/Praveent696/Table2Json
      * 
      */
     function makeJsonFromTable(tableId) {
	    var tbl = $('#' + tableId)	    
	    var tblhead = $(tbl).find('thead')
	    var tblbody = $(tbl).find('tbody')
	    var tblbodyCount = $(tbl).find('tbody>tr').length;
	    var header = [];
	    var JObjectArray = [];

	    $.each($(tblhead).find('tr>th'), function(i, j) {
	        header.push($(j).text())
	    })
	    $.each($(tblbody).find('tr'), function(key, value) {
	        var jObject = {};
	        for (var x = 0; x < header.length; x++) {	           
	        	let elem = $(this).find('td').eq(x);
	        	if(elem.attr('name')!='buttons'){
	        		jObject[elem.attr('name')] = elem.text()
	        	}
	        }
	        JObjectArray.push(jObject)

	    });
	    
	    return JObjectArray;
    }
     
    function registerAdittionalInfoCollector(){		
    	let elemForm = $('form#component').length >0 ? $('form#component') : $('form#sensor');    	
    	elemForm.submit(function( event ) {	    		 
    		 let tableFields = JSON.stringify(makeJsonFromTable('editable_aitn'))
    		 $('#additionalInfo').val(tableFields);
    	});			
	}
    
    if($('#editable_aitn')){		
	    $(function() {    	        
	    	$('#editable_aitn').setTableEditable({ $addButton: $('#but_add')});
	    	registerAdittionalInfoCollector();       
	    });
	}
