$(document).ready(function($){   
	var sensorId = $('#sensorId').text();
    var providerId = $('#providerId').text();
    var dataType = $('#dataType').text();
    var freq = $('#freq').text();
    var numOfIterations = $('#numOfIterations').text();
    var value = $('#value').text();
    var tokenAuth = $('#tokenAuth').text();
    var iteration = 0;
    
	$('#content').append('<strong>Inici de transmissió de dades</strong> <br>');
	$('#content').append('<hr>');
    
	setTimeout(sendData, 0);

	function sendData(){
		
		iteration = iteration + 1 ;
		
		var url = '/sentilo-demo/virtualsensor/send/'+sensorId+'/'+providerId+'/'+tokenAuth+'/';
		var valueToSend='';
		
		if(dataType == 'TEXT'){	
			valueToSend = value+'_'+iteration;
		}else if(dataType == 'NUMBER'){
			valueToSend = aleatorio(0,100);
		}else{
			var valueTo = aleatorio(0,2);
			if(valueTo == 0){
				valueToSend = 'false';
			}else{
				valueToSend = 'true';
			}
		}
		
		$.post(url + valueToSend)
    	.success(function(data) {

    		if(data == 'OK'){
	    		$('#content').append('Enviant dada: '+ valueToSend + '<br>');
	    			if (numOfIterations == iteration) {
	    				$('#content').append('<hr>');
	    				$('#content').append('<strong>Fi de transmissió de dades</strong>');
	    			}
    		}else{
    			$('#content').append('<div class="alert alert-error"><strong>Problemes en la transmissió: </strong><br><br>'
    									+ data +
    									'<br><br><strong>Es cancel·la la transmissió de dades</strong></div>');
    			numOfIterations = iteration;
    		}
    	});
		
		if (numOfIterations > iteration) { 
			setTimeout(sendData, freq);
		}
	}   
	
	function aleatorio(inferior,superior){
	    numPosibilidades = superior - inferior;
	    aleat = Math.random() * numPosibilidades;
	    aleat = Math.floor(aleat);
	    return parseInt(inferior) + aleat;
	} 
});