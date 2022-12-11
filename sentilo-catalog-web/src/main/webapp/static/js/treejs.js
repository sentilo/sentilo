/*!
 * jtree JavaScript Library v1.0
 *
 * Includes jquery.js
 * https://jquery.com/
 * 
 * Author : Mayank Pandey
 * 
 * Date: 2017-March-05 Time: 02:30
 *
 */

(function ( $ ) {

    var attributes      = null;
    var targetElement   = null;

    $.fn.treejs = function( options ) {

    	// get the reference of target element
    	targetElement = this;

    	attributes = $.extend({
            url 		: null,
			sourceType 	: 'html', 	// html, json. html is default
			initialState: 'close', 	// open, close. Default is close
            dataSource  : null,     // dataSource contains json data in it. It is used when sourceType is json
        }, options );
		
		// ajax call to get the tree nodes from a php script in case of sourceType = html
        if( attributes.sourceType == 'html' )
        {
            if( attributes.url == null )
            {
                alert('Error: missing ajax url');
                return false;
            }

    		$.ajax({
    			url : attributes.url,
    			method: 'get',
    			async: false,
    			data:
    			{
    				sourceType: attributes.sourceType
    			},
    			success: function(response)
    			{
    				$(targetElement).html(response);
    			}
    		});
        }
        // Otherwise iterate the dataSource and prepare the nodes
        else
        {
            if( attributes.dataSource == null )
            {
                alert('Error: Missing dataSource JSON object');
                return false;
            }

            var data = attributes.dataSource;
            var html = '';
            
            for(var i in data) {
                if( data.hasOwnProperty(i) ) {
					if (data[i].markAll && data[i].markAll !== null && data[i].markAll === "true") {
						html += '<ul class=" jtree_check_all_node category-group check-all-node"><li class="category"><label class="category-label"><input type="checkbox" id="check-all-input" parent-id="" class="jtree_check_all_checkbox category-input">'+ data[i]['parentNodeTxt'] +'</label></li></ul>';	
					} else {
						var parentIsActive = data[i]['active'];
						if (parentIsActive === 'true') {
							
							html += '<ul class="jtree_node jtree_parent_node category-group"> <li class="category"> <span class="jtree_expand jtree_node_open category-list-icon"> </span> <label class="category-label"><input type="checkbox" id="'+ data[i]['parentNodeId'] +'" parent-id="" class="jtree_parent_checkbox category-input"> '+ data[i]['parentNodeTxt'] +'</label>';
		                    html += '<ul class="jtree_node jtree_child_node types-group">';
		
		                    for(var j in data[i]['childNodes'])
		                    {
		                        if( data[i]['childNodes'].hasOwnProperty(j) )
		                        {
									var name = data[i]['childNodes'][j]['name'];
									var icon = data[i]['childNodes'][j]['icon'];
									var active = data[i]['childNodes'][j]['active'];
									if (active !== undefined && active === 'true') {
										if (icon !== undefined) {
											name = '<img class="type-icon" src="'+icon+'" alt="'+name+'" title="'+name+'">' + name;
										}
			                            html += '<li class="type-option"><label class="type-label"><input type="checkbox" id="'+ data[i]['childNodes'][j]['id'] +'" parent-id="'+ data[i]['parentNodeId'] +'" class="jtree_child_checkbox type-input"> '+ name +' </label></li>';
									}
		                        }
		                    }
		
		                    html += '</ul>';
		                    html += '</li></ul>';	
						}
					}                    
                }
            }

            $(targetElement).html( html );
        }

		// Initial state
		if( attributes.initialState == 'close' )
		{
			$('.jtree_child_node').hide();
			$('.jtree_expand').removeClass('jtree_node_open').addClass('jtree_node_close');
		}
		else
		{
			$('.jtree_child_node').show();
			$('.jtree_expand').removeClass('jtree_node_close').addClass('jtree_node_open');
		}

		// Show / Hide parent nodes
		$(document).on('click', '.jtree_expand', function(){
			if( $(this).hasClass('jtree_node_open') )
			{
				$(this).removeClass('jtree_node_open').addClass('jtree_node_close');
				$(this).next().next('ul').hide();
			}
			else
			{
				$(this).removeClass('jtree_node_close').addClass('jtree_node_open');
				$(this).next().next('ul').show();
			}
		});

		// Check / Uncheck all child for parent state change
		$(document).on('change', '.jtree_parent_checkbox', function(){
			
			if( $(this).is(':checked') )
			{
				var childUL = $(this).parent().next('ul');
				$(childUL).each(function(){
					$(this).find('li > label').find('input[type="checkbox"]').prop('checked', true);
					$.each($(this).find('li > label').find('input[type="checkbox"]'), (i,check) => {
						$(this).changeMultipleCheckboxesByMasterCheckValue(check.id, $(this).id, true);	
					});					
				});
				
				// Verify if all check all parents are checked > check all-check checkbox
				var parentsChecked = true;
				$('.jtree_parent_checkbox').each(function(i, checkbox) {
					parentsChecked &= $(checkbox).prop('checked');
				});
				$('.jtree_check_all_checkbox').prop('checked', parentsChecked);
			}
			else
			{
				var childUL = $(this).parent().next('ul');
				$(childUL).each(function(){
					$(this).find('li > label').find('input[type="checkbox"]').prop('checked', false);
					$.each($(this).find('li > label').find('input[type="checkbox"]'), (i,check) => {
						$(this).changeMultipleCheckboxesByMasterCheckValue(check.id, $(this).id, false);	
					});
				});	
				
				// Uncheck all-check checkbox
				$('.jtree_check_all_checkbox').prop('checked', false);
			}

		});
		
		// Check / uncheck parent if all child checked / unchecked
		$(document).on('change', '.jtree_child_checkbox', function(){
			var id = $(this).attr('id');
			var parentId = $(this).attr('parent-id');
			if( $(this).is(':checked') ) {
				var parent = $('#'+parentId+'.jtree_parent_checkbox');
				var childUL = $(parent).parent().next('ul');
				var parentChecked = true;
				$(childUL).each(function(){
					$(this).find('li > label').find('input[type="checkbox"]').each(function(i, check) {
						parentChecked &= $(check).prop('checked');	
					});					
				});	
				$(parent).prop('checked', parentChecked);
				
				// Verify if all childs are checked > check all parents > check all-check checkbox
				var parentsChecked = true;
				$('.jtree_parent_checkbox').each(function(i, checkbox) {
					parentsChecked &= $(checkbox).prop('checked');
				});
				$('.jtree_check_all_checkbox').prop('checked', parentsChecked);
			} else {
				$('.jtree_check_all_checkbox').prop('checked', false);
				$('#'+parentId+'.jtree_parent_checkbox').prop('checked', false);
				
			}
		});
		
		// On click search for same ids checkboxes and apply its own change to them
		$(document).on('click', '.jtree_child_checkbox', function(){
			var id = $(this).attr('id');
			var parentId = $(this).attr('parent-id');
			if( $(this).is(':checked') ) {
				$(this).changeMultipleCheckboxesByMasterCheckValue(id,parentId,true);
			} else {
				$(this).changeMultipleCheckboxesByMasterCheckValue(id,parentId,false);
			}
		});
		
		// Check / Uncheck all child for parent state change
		$(document).on('change', '.jtree_check_all_checkbox', function(){
			$('.jtree_parent_checkbox').prop('checked', $(this).prop('checked'));
			$('.jtree_child_checkbox').prop('checked', $(this).prop('checked'));
		});

    };

    // To get the Parent, Child : Checked, Unchecked nodes 
    $.fn.extend({
        getCheckedParentNodes: function() {
            return $(this).find('.jtree_parent_checkbox:checked');
        },
        getUncheckedParentNodes: function() {
            return $(this).find('.jtree_parent_checkbox:not(:checked)'); 
        },
        getCheckedChildNodes: function() {
            return $(this).find('.jtree_child_checkbox:checked');
        },
        getUncheckedChildNodes: function() {
            return $(this).find('.jtree_child_checkbox:not(:checked)');  
        }
    });
    
    // Check & uncheck functions
	$.fn.extend({
        uncheckAll: function() {
			$('.jtree_check_all_checkbox').prop('checked', '');
			$('.jtree_parent_checkbox').prop('checked', '');
			$('.jtree_child_checkbox').prop('checked', '');
		},
		checkAll: function() {
			$('.jtree_check_all_checkbox').prop('checked', 'checked');
			$('.jtree_parent_checkbox').prop('checked', 'checked');
			$('.jtree_child_checkbox').prop('checked', 'checked');
		},
		changeMultipleCheckboxesByMasterCheckValue: function(id, parentId, checked) {
			$('#'+id+'.jtree_child_checkbox').not('[parent-id="'+parentId+'"]').prop('checked',checked)
			$('#'+id+'.jtree_child_checkbox').not('[parent-id="'+parentId+'"]').trigger('change');
		}
    });

    // To refresh the tree
    $.fn.extend({
        refresh: function() {
            if( attributes.sourceType == 'html' )
            {
                if( attributes.url == null )
                {
                    alert('Error: missing ajax url');
                    return false;
                }

                $.ajax({
                    url : attributes.url,
                    method: 'get',
                    async: false,
                    data:
                    {
                        sourceType: attributes.sourceType
                    },
                    success: function(response)
                    {
                        $(targetElement).html(response);
                    }
                });
            }
            else
            {
                if( attributes.dataSource == null )
                {
                    alert('Error: Missing dataSource JSON object');
                    return false;
                }

                var data = attributes.dataSource;
                var html = '';

                for(var i in data) {
	                if( data.hasOwnProperty(i) ) {
						if (data[i].markAll && data[i].markAll !== null && data[i].markAll === "true") {
							html += '<ul class="jcategory-group jtree_check_all_node check-all-node"><li class="category"><label class="category-label"><input type="checkbox" id="check-all-input" parent-id="" class="jtree_check_all_checkbox category-input">'+ data[i]['parentNodeTxt'] +'</label></li></ul>';	
						} else {
							var parentIsActive = data[i]['active'];
							if (parentIsActive === 'true') {
								html += '<ul class="jtree_node jtree_parent_node category-group"> <li class="category"> <span class="jtree_expand jtree_node_open category-list-icon"> </span> <label class="category-label"><input type="checkbox" id="'+ data[i]['parentNodeId'] +'" parent-id="" class="jtree_parent_checkbox category-input"> '+ data[i]['parentNodeTxt'] +'</label>';
			                    html += '<ul class="jtree_node jtree_child_node types-group">';
			
			                    for(var j in data[i]['childNodes'])
			                    {
			                        if( data[i]['childNodes'].hasOwnProperty(j) )
			                        {
										var name = data[i]['childNodes'][j]['name'];
										var icon = data[i]['childNodes'][j]['icon'];
										var active = data[i]['childNodes'][j]['active'];
										if (active !== undefined && active === 'true') {
											if (icon !== undefined) {
												name = '<img class="type-icon" src="'+icon+'" alt="'+name+'" title="'+name+'">' + name;
											}
				                            html += '<li class="type-option"><label class="type-label"><input type="checkbox" id="'+ data[i]['childNodes'][j]['id'] +'" parent-id="'+ data[i]['parentNodeId'] +'" class="jtree_child_checkbox type-input"> '+ name +' </label></li>';
										}
			                        }
			                    }
			
			                    html += '</ul>';
			                    html += '</li></ul>';	
							}
						}                    
	                }
	            }

                $(targetElement).html( html );
            }
        }
    });

}( jQuery ));