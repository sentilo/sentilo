	var callBackRowFunction = function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
		$("input.convertToToggle", nRow).bootstrapToggle();
	};
	var lastColumnRender = function(data, type, row) {
		return '<input type="checkbox" class="convertToToggle" name="grant" value="'+notDefaultPermission+'" data-off="'+sectorGrantRead+'" data-on="'+sectorGrantAdmin+'" />';
	};
	var nameColumnRender = function(data, type, row) {
		return '<label title="'+row[0]+'">'+data+'</label>';
	};
	var otherColumnsRender = [{"func": nameColumnRender, "pos": 1, "sortable": true},{"func":lastColumnRender, "pos": 3, "sortable": false}];

	function showAddToSectorModalWindow(popupSelector) {
		$(popupSelector).modal('show');
	}
	
	function dismissAddToSectorModal(popupSelector) {
		$(popupSelector).modal('hide');
	}
	
	function addElementsToSector(tableSelector, postUrl, tablesToReload, popupSelector, confirmMessage) {
		var checkedBoxes = $("table#"+tableSelector+"").find("input[name='selectedIds']:checked");
		if(checkedBoxes.length > 0) {
			var jsonData = [];
			$(checkedBoxes).each((index,element) => {
				var parentRow = $(element).parents("tr")[0];
				var checkPermission = $(parentRow).find("div.toggle")[0]
				var checkedAdmin = $(checkPermission).hasClass("off") ? defaultPermission: notDefaultPermission;
				jsonData.push(
					{
						"grant" : checkedAdmin,
						"selectedId" : $(element).val()
					}
				);
			});
			if(jsonData.length > 0) {
				jsonPOST(postUrl, JSON.stringify(jsonData), function(data) {
					if(data.result === 'OK') {
						tablesToReload.forEach(el => reload(el));
						dismissAddToSectorModal(popupSelector)
						showConfirmNotification('', confirmMessage);
					}
				})
			}else{
				dismissAddToSectorModal(popupSelector);
			}
		}
		
	}