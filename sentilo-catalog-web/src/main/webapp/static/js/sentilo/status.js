function refreshStatus(platformStatus){			
	if(platformStatus.items){
		if(platformStatus.isPlatformRunningProperly){
			$('#globalPlatformMessage').append(globalPlatformMsgOk);
			$('#globalPlatformPanel').addClass('alert alert-success');
			$('#globalPlatformIconStatus').addClass('icon-ok');
		}else{
			$('#globalPlatformMessage').append(globalPlatformMsgKo);
			$('#globalPlatformPanel').addClass('alert alert-error');
			$('#globalPlatformIconStatus').addClass('icon-warning-sign');
		}
		
		$.each(platformStatus.items, function( index, item ) {
			var div = $('#hidden-template').html().replace(/item/g,item.id).replace("##name##", item.name).replace("##description##",item.description).replace("##desc##", item.stateDesc);
			
			if(item.status){
				div = div.replace("##icon##", "icon-ok").replace("##desc-style##","alert-success");
			}else{
				div = div.replace("##icon##", "icon-warning-sign").replace("##desc-style##","alert-error");
			}						
			
			$('#platformStatus').append(div);
		});		
	}	
}
