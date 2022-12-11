    function showAddUserModalWindow() {
		resetErrorList();
    	$('#usersNotInSectorModal').modal('show');
    }

    function dismissModal() {
		resetErrorList();
        $('#usersNotInSectorModal').modal('hide');
    }

	function resetErrorList() {
        var errorList = $('#error-list');
        var errorContainer = $('#error-container');
        errorContainer.hide();
        errorList.find('li')
            .remove()
            .end();
    }
	
	function addToSector(tableSelector,sAjaxSource) {
		resetErrorList();
		var selectedIds = [];
		
		$("table#"+tableSelector+" input[name='selectedIds']:checked").each(function(i,el) {
			selectedIds.push($(el).val());
		});
		
		if(selectedIds.length > 0) {
			var jsonStr = JSON.stringify({selectedIds: selectedIds});			
			jsonPOST(sAjaxSource, jsonStr, function(data){
				if(data.result === 'OK') {
					reload(tableUser_inSector);
					reload(tableUser_notInSector);
					dismissModal();				
					showConfirmNotification('', sectorConfirmAssignedUsers);
				}});
		}else{
			dismissModal();
		}
	}
	
	function checkSectorsAndDelete(urlCheck, tableSelector, userForm, checkMessage, confirmPopupDeleteMessage) {
		var selectedIds = [];		
		$("table#"+tableSelector+" input[name='selectedIds']:checked").each(function(i,el) {
			selectedIds.push($(el).val());
		});
		if(selectedIds.length > 0) {
			var jsonStr = JSON.stringify({selectedIds: selectedIds});		
			jsonPOST(urlCheck, jsonStr, function(data) {
				if(data.selectedIds != undefined && data.selectedIds.length > 0) {
					checkMessage += '<br /><ul>';
					data.selectedIds.forEach(el => checkMessage += '<li>'+el+'</li>');
					checkMessage += '</ul>';
					deleteSelected(userForm, checkMessage);
				} else {
					deleteSelected(userForm, confirmPopupDeleteMessage);
				}
			});		
		} else {
			deleteSelected(userForm, confirmPopupDeleteMessage);
		}
	}