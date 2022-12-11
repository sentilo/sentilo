$(document).ready(function() {	
    $(".convertToToggle").bootstrapToggle();

    $("#checkAllSectorial").change(function() {
        if($(this).is(":checked")) {
            $(".idSectorCheckbox").each(function(i, el) {
                if(!$(el).is(":checked")) $(el).click();
            });
        } else {
            $(".idSectorCheckbox").each(function(i, el) {
                if($(el).is(":checked")) $(el).click();
            });
        }
    });

    $("a.btn-success").click(function(e) {
        e.preventDefault();
        if($(".idSectorCheckbox:checked").length == 0) {
            bootbox.dialog(selectOneErrorMsg, [{
                'label' : okButtonLabelMsg,
                'class' : 'btn-danger'
            }]);
        } else {
            var form = $(this).parents("form")[0];
            $(form).find("input[name^='sectors[']");
            $(".idSectorCheckbox:checked").each(function(i, el) {
                var baseName = 'sectors['+i+']';
                var finalIdName = baseName;
                if($(".convertToToggle").length > 0) {
                    //make input grants
                    var tr = $(el).parents("tr")[0];
                    var parentRow = $(el).parents("tr")[0];
                    var checkPermission = $(parentRow).find("div.toggle")[0];        
                    var grant = $(checkPermission).hasClass("off") ? sectorReadGrant : sectorAdminGrant;
                    $(form).append("<input type='hidden' name='"+baseName+".grant' value='"+grant+"' />");
                    finalIdName = finalIdName + ".sectorId";
                } 
                $(form).append("<input type='hidden' name='"+finalIdName+"' value='"+$(el).val()+"' />");
            });
            $(form).submit();
        }
    });
});

