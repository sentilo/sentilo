var isAdmin = false;
$(document).ready(function() {
	isAdmin = $("body").hasClass("sntl-admin");
});

///////////////////////////////////////////////////
//
//	AUDIO PLAYER SCRIPTS
//
// I'm using wavesurfer.js v.2.0.6
// https://wavesurfer-js.org

var wavesurfer;
function showAndLoadAudioPlayer(panel, link, extendedLink, sensor, sensorType, timestamp) {
	
	// Add sensor & type
	$(panel).append('<span class="player-sensor-info">' + sensor + '</span><br class="player-sensor-info"/><span class="player-sensor-info">' + sensorType + '</span><br class="player-sensor-info"/><br class="player-sensor-info"/>');
	$(panel).append('<span class="player-sensor-value-link">' + link + '</span>');
	
	if (wavesurfer && wavesurfer !== null && wavesurfer !== 'undefined') {
		wavesurfer.stop();
		wavesurfer.empty();
		wavesurfer.destroy();
		wavesurfer.unAll();
	}
	
	// Clear possible old elements
	$("#audio-player-wrapper").remove();
	
	var html = 
		'<div id="waveform-error-wrapper" style="display: none"><p class="error-message"></p></div>' +
		'<div id="audio-player-wrapper">' +
		'	<div id="waveform"></div>' +
		'	<div id="controller-wrapper">' +
		'		<div id="controller-buttons" class="btn-group">' +
		'			<div id="play-btn" class="btn btn-small"><i class="icon-play"></i></div>' + 
		'			<div id="stop-btn" class="btn btn-small"><i class="icon-stop"></i></div>' + 
		'		</div>' +
		'		<div id="controller-info">' + 
		'			<p><strong>Ellapsed: </strong><span id="ellapsed-time"></span></p>' + 
		'		</div>' +
		' 	</div>' +
		'</div>' +
		'<hr class="file-date-separator" />' +
		'<div id="file-info-wrapper" class="player-sensor-info">' +
		'	<span id="file-date"></span>' +
		'</div>';
		
	// Add all
	$(panel).append(html);
	
	// Create the waveform
	wavesurfer = WaveSurfer.create({
	    container: '#waveform',
	    waveColor: '#1abc9c',
	    progressColor: '#12715f'
	});
	
	// Player controller events
	$("#play-btn").click(function() {
		wavesurfer.playPause();
		updateAudioButtons();
	});
	
	$("#stop-btn").click(function() {
		wavesurfer.stop();
		updateAudioButtons();
	});
	
	// Load!!!
	loadAudioPlayer(link, extendedLink, timestamp);
	
	// Set the audio properties
	$("#file-date").html(timestamp);
	$("#ellapsed-time").html('0.000s / ' + wavesurfer.getDuration().toFixed(3) + 's');
	
	// Player events
	wavesurfer.on('ready', function() {
		showAudioPlayer();
		$("#ellapsed-time").html('0.000s / ' + wavesurfer.getDuration().toFixed(3) + 's');
	});
	
	wavesurfer.on('audioprocess', function() {
		$("#ellapsed-time").html(wavesurfer.getCurrentTime().toFixed(3) + 's / ' + wavesurfer.getDuration().toFixed(3) + 's');
	});
	
	wavesurfer.on('interaction', function() {
		$("#ellapsed-time").html(wavesurfer.getCurrentTime().toFixed(3) + 's / ' + wavesurfer.getDuration().toFixed(3) + 's');
	});
	
	wavesurfer.on('error', function(message) {
		hideAudioPlayer();
		$("#waveform-error-wrapper .error-message").html('<strong>ERROR: </strong> ' + message);
		$("#waveform-error-wrapper").show('slow');
	});
	
}

function showAudioButtons() {
	$("#controller-buttons").show();
	$("#controller-info").show();
}

function hideAudioButtons() {
	$("#controller-buttons").hide();
	$("#controller-info").hide();
}

function showAudioPlayer() {
	$("#audio-player-wrapper").show();
	showAudioButtons();
}

function hideAudioPlayer() {
	$("#audio-player-wrapper").hide();
	hideAudioButtons();
}

function updateAudioButtons() {
	if (wavesurfer.isPlaying()) {
		var btn = $(".icon-play"); 
		btn.removeClass("icon-play");
		btn.addClass("icon-pause");
	} else {
		var btn = $(".icon-pause"); 
		btn.removeClass("icon-pause");
		btn.addClass("icon-play");
	}
}

function loadAudioPlayer(link, extendedLink, timestamp) {
	
	if (!wavesurfer || wavesurfer === null || wavesurfer === 'undefined') {
		return;
	}
		
	// Init player
	wavesurfer.stop();
	wavesurfer.empty();
	
	// Load audio
	wavesurfer.load(extendedLink);

	if (isAdmin) {
		// If is admin panel, update last date value!
		$("#current-obs-timestamp").html(timestamp);
		// Set the link info
		$(".player-sensor-value-link").html(link);
	}
	
	// Set the audio properties
	$("#file-date").html(timestamp);
	updateAudioButtons();

}

function stopAudioPlayer() {
	if (wavesurfer && wavesurfer !== null && wavesurfer !== 'undefined') {
		wavesurfer.stop();
		wavesurfer.empty();
		wavesurfer.destroy();
		wavesurfer.unAll();
	}
	
	// Clear possible old elements
	$("#audio-player-wrapper").remove();
}

///////////////////////////////////////////////////
//
//	VIDEO PLAYER SCRIPTS
//
//	I'm using HTML5 Video Player
//
function showAndLoadVideoPlayer(panel, link, extendedLink, sensor, sensorType, timestamp) {
	
	// Add sensor & type
	$(panel).append('<span class="player-sensor-info">' + sensor + '</span><br class="player-sensor-info"/><span class="player-sensor-info">' + sensorType + '</span><br class="player-sensor-info"/><br class="player-sensor-info"/>');		
	$(panel).append('<span class="player-sensor-value-link">' + link + '</span>');
	
	// Clear possible old elements
	$("#video-player-wrapper").remove();
	
	var html = 	
		'<div id="video-player-wrapper">' +
		'	<video id="video-player" playsinline controls>' +
		'		<source id="video-src">' +
		'	</video>' +
		'</div>' +
		'<hr class="file-date-separator" />' +
		'<div id="controller-wrapper" class="player-sensor-info">' +
		' 	<div id="file-info-wrapper">' +
		'		<span id="file-date"></span>' +
		' 	</div>' +
		'</div>';
	
	// Add all
	$(panel).append(html);
	
	// Load video
	loadVideoPlayer(link, extendedLink, timestamp)
}

function loadVideoPlayer(link, extendedLink, timestamp) {

	if ( $('#video-player-wrapper').length ) {
		// Set the video properties
		$("#file-date").html(timestamp);
		
		// Set the video src
		$("#video-src").attr('src', extendedLink);
		
		// Load the video
		$('#video-player').load();
	}	

	if (isAdmin) {
		// If is admin panel, update last date value!
		$("#current-obs-timestamp").html(timestamp);
		// Set the link info
		$(".player-sensor-value-link").html(link);
	}
	
}

function stopVideoPlayer() {
	var videoPlayer = document.getElementById("video-player");
	if (videoPlayer) {
		videoPlayer.stop();
		$("#video-player-wrapper").remove();
	}
}

///////////////////////////////////////////////////
//
//	IMAGE VIEWER SCRIPTS
//
function showImage(panel, link, extendedLink, sensor, sensorType, timestamp) {

	// Add sensor & type
	$(panel).append('<span class="player-sensor-info">' + sensor + '</span><br class="player-sensor-info"/><span class="player-sensor-info">' + sensorType + '</span><br class="player-sensor-info"/>');
	$(panel).append('<span class="player-sensor-value-link">' + link + '</span>');
	
	// Create html node
	var html = 
		'<div id="image-player-wrapper">' +
		'	<div id="image-wrapper"></div>' +
		'</div>' +
		'<hr class="file-date-separator" />' +
		'<div id="controller-wrapper" class="player-sensor-info">' +
		'	<div id="file-info-wrapper">' +
		'		<span id="file-date"></span>' +
		'	</div>' +
		'</div>';
	
	// Add html
	$(panel).append(html);
	
	// Set the audio properties
	$("#file-date").html(timestamp);
	
	loadImage(link, extendedLink, timestamp);
}

function loadImage(link, extendedLink, timestamp) {
	
	$("#image-wrapper").empty();
	
	var img = $('<img id="link-image" />').attr('src', extendedLink)
	.on('load', function() {
		if (!this.complete || typeof this.naturalWidth == "undefined" || this.naturalWidth == 0) {
			img.attr('src', getErrorImage());
            $("#image-wrapper").html(img);
        } else {
        	var imageLink = $('<a id="image-link" href="'+extendedLink+'" data-lightbox="link-image"></a>');
        	$(imageLink).append(img);
            $("#image-wrapper").html(imageLink);
        }
	});
	
	// Set the image properties
	$("#file-date").html(timestamp);
	
	if (isAdmin) {
		// If is admin panel, update last date value!
		$("#current-obs-timestamp").html(timestamp);
		// Set the link info
		$(".player-sensor-value-link").html(link);
	}
	
}

function getErrorImage() {
	var contextPath = window.location.pathname.substring(0, window.location.pathname.indexOf("/",2));
	return contextPath + '/static/img/NoPhotoFull.jpg';
}


///////////////////////////////////////////////////
//
//	FILE DOWLOADER SCRIPTS
//
function showFile(panel, link, extendedLink, filename, sensor, sensorType, timestamp) {
	
	// Add sensor & type
	$(panel).append('<span class="player-sensor-info">' + sensor + '</span><br class="player-sensor-info"/><span class="player-sensor-info">' + sensorType + '</span><br class="player-sensor-info"/><br class="player-sensor-info"/>');
	$(panel).append('<span class="player-sensor-value-link">' + link + '</span>');
	
	// Create html node
	var html = 
		'<div id="file-downloader-wrapper">' +
		'	<div id="file-wrapper">' +
		' 		<a id="file-link" class="btn btn-success" href="'+extendedLink+'" target="_blank" download="'+filename+'">Download ' + filename + '</a>' +
		'	</div>' +
		'</div>' +
		'<hr class="file-date-separator" />' +
		'<div id="controller-wrapper">' +
		'	<div id="file-info-wrapper" class="player-sensor-info">' +
		'		<span id="file-date"></span>' +
		' 	</div>' +
		'</div>'; 
	
	// Add html
	$(panel).append(html);
	
	// Set the file properties
	$("#file-date").html(timestamp);
	
}

function loadFile(link, extendedLink, filename, timestamp) {
	if ( $('#file-downloader-wrapper').length ) {
		// Set the file properties
		$("#file-date").html(timestamp);
		$("#file-link").attr('href', extendedLink);
		$("#file-link").attr('download', filename);
		$("#file-link").html('Dowload ' + filename);
	}
	
	if (isAdmin) {
		// If is admin panel, update last date value!
		$("#current-obs-timestamp").html(timestamp);

		// Set the link info
		$(".player-sensor-value-link").html(link);
	}
	
}

function downloadFile(fileURL, fileName) {
	var link = document.createElement('a');
	link.setAttribute('href', fileURL);
	link.setAttribute('download', fileName);
	link.setAttribute('target', '_blank');
	link.style.display = 'none';
	document.body.appendChild(link);
	link.click();
	document.body.removeChild(link);
}


///////////////////////////////////////////////////
//
//	LINK SCRIPTS
//
function showLink(panel, link, sensor, sensorType, timestamp) {

	// Add sensor & type
	$(panel).append('<span>' + sensor + '</span><br/><span>' + sensorType + '</span><br/><br/>');
	
	// Create html node
	var html = 
		'<div id="link-wrapper">' +
		'	<div id="file-wrapper">' +
		' 		<a id="link" class="" href="'+link+'" target="_blank" >'+link+'</a>' +
		'	</div>' +
		'</div>'+
		'<hr class="file-date-separator" />'
		'<div id="controller-wrapper">' +
		'	<div id="file-info-wrapper">' +
		'		<span id="file-date"></span>' +
		' 	</div>' +
		'</div>';
	
	// Add html
	$(panel).append(html);
	
	// Set the file properties
	$("#file-date").html(timestamp);
	
}

///////////////////////////////////////////////////
//
//	JSON SCRIPTS
//
function showJson(panel, value, timestamp) {

	$(panel)
		.jsonPresenter('destroy')
		.jsonPresenter({ json: jQuery.parseJSON(value) })
		.jsonPresenter('expandAll');

	var actionButtonsToolbar = $('<div class="btn-toolbar pull-right"><i class="icon-align-left"></i></a>');
	var actionButtonsGrp = $('<div class="btn-group" />');
			
	// Collapse all button
	actionButtonsGrp.append($('<a class="btn" href="#" id="collapse-all"><i class="icon-resize-small"></i></a>')
		.on('click', function() {
			$(panel).jsonPresenter('collapseAll');
		})
	);
	
	// Expand all button
	actionButtonsGrp.append($('<a class="btn" href="#" id="expand-all"><i class="icon-resize-full"></i></a>')
		.on('click', function() {
			$(panel).jsonPresenter('expandAll');
		})
	);
	
	// Expand n levels button
	actionButtonsGrp.append($('<a class="btn" href="#" id="expand-levels"><i class="icon-align-left"></i></a>')
		.on('click', function() {
			var levels = parseInt($('#levels').val());
			$(panel).jsonPresenter('expand', levels);
		})
	);
	
	// Expandable levels
	actionButtonsToolbar.append($('<input type="text" id="levels" value="0" class="pull-right" />'));
	
	// Append buttons
	actionButtonsToolbar.append(actionButtonsGrp);
	$(panel).append(actionButtonsToolbar);
	
	if (isAdmin) {
		// If is admin panel, update last date value!
		$("#current-obs-timestamp").html(timestamp);
	}
}
