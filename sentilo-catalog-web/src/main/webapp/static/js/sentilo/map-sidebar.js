// Global variables
var treeData = {};
var treeConfig = {};
var nonTypeId = '@!__###__!@';
var typesFilterTimeout = 250;
var typesSearchTimeout = 500;
var typesFilterTimer;
var typesSearchTimer;
var treeInitialized = false;

// Search values, initial none
var ct = [nonTypeId];

function showOpenButton() {
	if ($('body').hasClass('sntl-map'))
		$("#map-sidebar-menu-button").show();
	else
		$("#map-sidebar-menu-button").hide();
}

function openMapSidebar() {
	// Close component modal layer if opened
	hideModalLayer();
	// Show sidebar
	document.getElementById('map-sidebar').style.left = '0px';
}

function closeMapSidebar() {
	// Hide sidebar
	document.getElementById('map-sidebar').style.left = '-40%';
}

function initMapSidebar() {
	
	showOpenButton();
	initOpenCloseEvents();
	createTree();
	initFilterInput();
		
	// Next, display tree checks selected depending on request params
	initTreeCheckboxes();
		
	// Finally, define one listener for each click on tree checkbox
	// Add filter handler to all checkboxes
	$('#tree input[type=checkbox]').click(function(e) {
		filterTypes();
	});
	
	treeInitialized = true;	
}
	
function initOpenCloseEvents() {
    $('body').on('modal-layer-opened', function() {
    	closeMapSidebar();
   	});
    $('body').on('modal-layer-closed', function() {
        // Do nothing here
   	});
}

function initTreeCheckboxes() {			
	if (treeConfig.initialFilterIds !== '' || treeConfig.initialFilterCategories !== ''){
		if (treeConfig.initialFilterIds !== '') {
			let selectedIds = treeConfig.initialFilterIds.split(',');
			$.each(selectedIds, function(i, type) {
				const typeId = type.trim();
				$('#'+typeId+'.jtree_child_checkbox').prop('checked', true);				
				$('#'+typeId+'.jtree_child_checkbox').trigger('change');
			});
		}
		
		if(treeConfig.initialFilterCategories !== ''){
			let selectedCategories = treeConfig.initialFilterCategories.split(',');
			$.each(selectedCategories, function(i, category) {
				let categoryId = normalizeTag(category);
				$('#'+categoryId+'.jtree_parent_checkbox').prop('checked', true);			
				$('#'+categoryId+'.jtree_parent_checkbox').trigger('change');				
			});			
		}
		
	}else{
		$('#tree').checkAll();
	}				
}	

function initFilterInput() {
	$('#searchByType').keyup(function() {
		clearTimeout(typesSearchTimer);
		typesSearchTimer = setTimeout(function() {
			searchTypes();	
		}, typesSearchTimeout);
	});
}

function createTree() {
	$('#tree').treejs({
		sourceType: 'json',
		dataSource: treeData,
		initialState: treeConfig.initialState
	});
}

function getTreeSelectedIds() {	
	var selectedIds = [];

		var selectedItems = $('#tree').getCheckedChildNodes();
		var checkAll = $('#check-all-input').prop('checked');
		if (checkAll === true) {
			// Search for all options, no filters needed
			selectedIds = [];
		} else if (selectedItems.length > 0) {
			// Search for a few options
			// Iterate foreach checked option
			$(selectedItems).each(function(index) {
				selectedIds.push($(this).prop('id'));
			});	
		} else {
			// Search for none, use an special non used typology id
			selectedIds.push(nonTypeId);
		}
	
	
	// Clean duplicates
	var noDupsIds = [...new Set(selectedIds)];
	return noDupsIds;
}
	
function filterTypes() {
	clearTimeout(typesFilterTimer);
	typesFilterTimer = setTimeout(function() {
		infowindowMustBeClosed = true;
		closeInfoWindow();
		getPlaces();
	}, typesFilterTimeout);
}

function searchTypes() {
	clearTimeout(typesSearchTimer);
	typesSearchTimer = setTimeout(function() {
		var searchValue = $('#searchByType').val().trim();
		if (searchValue) {
			var nodesToShow = [];
			$('.jtree_node').hide();
			$('.type_option').hide();
			$.each(treeData, function(i, parentNode) {
				if (parentNode.parentNodeTxt.toUpperCase().indexOf(searchValue.toUpperCase()) !== -1) {
					// Show main category
					$('#'+parentNode.parentNodeId).parents("*").show();
				}
				if (parentNode.childNodes) {
					$.each(parentNode.childNodes, function(j, childNode) {
						if (childNode.name.toUpperCase().indexOf(searchValue.toUpperCase()) !== -1) {
							// Show option
							$('#'+childNode.id).parents("*").show();
						} else {
							// Hide option
							$('#'+childNode.id).parent().parent().hide();
						}
					});
				}
			});
		} else {
			$('#tree [style*="display: none"]').show().children().show();
		}
	}, typesSearchTimeout);
}
	
function getTreeData() {
	
	var allCategories = {
		'check-all': {
			'parentNodeId': '0',
			'parentNodeTxt': treeConfig.translations.checkall.text,
			'markAll': 'true',
			'active': 'true'
		},
	};
	
	var categories = {};
	var noneCategories = {};
	$.each(componentTypes, function(i, type) {
		if (type.tags && type.tags !== '') {
			$.each(type.tags.split(','), function(j, tag) {
				var normalizedTag = normalizeTag(tag);
				if (!categories[normalizedTag]) {
					categories[normalizedTag] = {
						'parentNodeId': normalizedTag,
						'parentNodeTxt': tag,
						'active': 'true',
						'childNodes': []
					};
				} 
				categories[normalizedTag].childNodes.push({
					id: type.id,
					name: type.name,
					icon: type.icon,
					active: 'true'
				});	
			})
		} else {
			if (!noneCategories.none) {
				noneCategories.none = {
					'parentNodeId': 'none',
					'parentNodeTxt': treeConfig.translations.categories.none,
					"active": "true",
					"childNodes": []
				};
			} 
			noneCategories.none.childNodes.push({
				id: type.id,
				name: type.name,
				icon: type.icon,
				active: 'true'
			});	
		}
	});
	
	const orderedCategories = Object.keys(categories).sort().reduce(
	  (obj, key) => { 
	    obj[key] = categories[key]; 
	    return obj;
	  }, 
	  {}
	);
	
	var data = {
		...allCategories,
		...orderedCategories,
		...noneCategories
	}
	
	return data;
	
}

function normalizeTag(text) {
    return text.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g,"").replace(' ', '_');
    // return utf8_to_b64(text);
}

function utf8_to_b64(str) {
  return window.btoa(unescape(encodeURIComponent(str)));
}

function b64_to_utf8(str) {
  return decodeURIComponent(escape(window.atob(str)));
}