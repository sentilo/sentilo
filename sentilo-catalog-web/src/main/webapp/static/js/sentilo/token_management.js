    
function regenerateToken(idSelector, serviceUrl, tokenSelector, confirmMessage) {
    bootbox.confirm(confirmMessage, function(result) {
		if(result == true) {
            const idValue = $(idSelector).val();
            jsonPOST(serviceUrl, JSON.stringify({id: idValue}),function(data) {
                if(data && data.status === 'OK') {
                    $(tokenSelector).val(data.token);
                } else {
                    showErrorNotification(data.errorTittle, data.errorBody);
                }
            });
		}
	});
}