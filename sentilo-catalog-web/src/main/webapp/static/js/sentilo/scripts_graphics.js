var isAdmin = false;
$(document).ready(function() {
	isAdmin = $("body").hasClass("sntl-admin");
});

var data = [];
var xAxisLabels = [];
var from = null;
var to = null;
var oldToList = [];
var theFuture = null;


/**
 * Makes link chart
 */
 
var audioLinkObjects = [];
function makeAudioLinkChartLine(id, ph, data) {
	
	var html = 
		'<div class="activity_text_element">' +
		'	<span class="label label-info">' + data.timestamp + '</span>&nbsp;' +
		'  	<span class="label label-success">AUDIO</span>&nbsp;' + 
		' 	<span class="link"><a id="audio-link-'+id + '" class="audio-link" href="#" onclick="loadAudioPlayer(audioLinkObjects['+id+'].value, audioLinkObjects['+id+'].formattedValue, audioLinkObjects['+id+'].timestamp)">' + data.value + '</a></span>' +
		'</div>';
	
	ph.append(html);
	
	audioLinkObjects[id] = {
		value: data.value,
		formattedValue: data.formattedValue,
		timestamp: data.timestamp
	};
	
};

var videoLinkObjects = [];
function makeVideoLinkChartLine(id, ph, data) {
	
	var html = 
		'<div class="activity_text_element">' +
		'	<span class="label label-info">' + data.timestamp + '</span>&nbsp;' +
		'  	<span class="label label-success">VIDEO</span>&nbsp;' + 
		' 	<span class="link"><a id="video-link-'+id + '" class="video-link" href="#" onclick="loadVideoPlayer(videoLinkObjects['+id+'].value, videoLinkObjects['+id+'].formattedValue, videoLinkObjects['+id+'].timestamp)">' + data.value + '</a></span>' +
		'</div>';
	
	ph.append(html);
	
	videoLinkObjects[id] = {
		value: data.value,
		formattedValue: data.formattedValue,
		timestamp: data.timestamp
	};
	
};

var imageLinkObjects = [];
function makeImageLinkChartLine(id, ph, data) {
	
	var html = 
		'<div class="activity_text_element">' +
		'	<span class="label label-info">' + data.timestamp + '</span>&nbsp;' +
		'  	<span class="label label-success">IMAGE</span>&nbsp;' + 
		' 	<span class="link"><a id="image-link-'+id + '" class="image-link" href="#" onclick="loadImage(imageLinkObjects['+id+'].value, imageLinkObjects['+id+'].formattedValue, imageLinkObjects['+id+'].timestamp)">' + data.value + '</a></span>' +
		'</div>';
	
	ph.append(html);
	
	imageLinkObjects[id] = {
		value: data.value,
		formattedValue: data.formattedValue,
		timestamp: data.timestamp
	};
	
};

var fileLinkObjects = [];
function makeFileLinkChartLine (id, ph, data) {
	
	var html = 
		'<div class="activity_text_element">' +
		'	<span class="label label-info">' + data.timestamp + '</span>&nbsp;' +
		'  	<span class="label label-success">FILE</span>&nbsp;' + 
		' 	<span class="link"><a id="file-link-'+id + '" class="file-link" href="#" onclick="loadFile(fileLinkObjects['+id+'].value, fileLinkObjects['+id+'].formattedValue, fileLinkObjects['+id+'].filename, fileLinkObjects['+id+'].timestamp)">' + data.value + '</a></span>' +
		'</div>';
	
	ph.append(html);
	
	fileLinkObjects[id] = {
		value: data.value,
		formattedValue: data.formattedValue,
		filename: data.value.substring(data.value.lastIndexOf('/')+1),
		timestamp: data.timestamp	
	};
};

function makeLinkChartLine(id, ph, data) {
	
	var html = 
		'<div class="activity_text_element">' +
		'	<span class="label label-info">' + data.timestamp + '</span>&nbsp;' +
		'  	<span class="label label-success">LINK</span>&nbsp;' + 
		' 	<span class="link"><a id="link-'+id + '" class="link" href="'+data.value+'" target="_blank">' + data.value + '</a></span>' +
		'</div>';
	
	ph.append(html);

};

function makeLinkChart(placeholder, url, linkType, callback) {

	var ph = $(placeholder);

	jsonGET(url, [], function(dto) {
		
		$(placeholder).empty();		
		
		$.each(dto.events, function(key, val) {
			if (linkType === 'AUDIO_LINK') {
				makeAudioLinkChartLine(key, ph, val);	
			} else if (linkType === 'VIDEO_LINK') {
				makeVideoLinkChartLine(key, ph, val);	
			} else if (linkType === 'IMAGE_LINK') {
				makeImageLinkChartLine(key, ph, val);	
			} else if (linkType === 'FILE_LINK') {
				makeFileLinkChartLine(key, ph, val);	
			} else if (linkType === 'LINK') {
				makeLinkChartLine(key, ph, val);	
			} 
		});
		
		// if callback exist execute it
		callback && callback();
	});
};


/**
 * Makes text chart
 */
 
function makeTextChartLine(ph, timestamp, text, label) {
	
	var html = 
		'<div class="activity_text_element">' +
		'	<span class="label label-info">' + timestamp + '</span>&nbsp;' +
		'  	<span class="label label-success">TEXT</span>&nbsp;' + 
		' 	<span class="text">' + text + '</span>' +
		'</div>';
	
	ph.append(html);
	
};

var jsonLinkObjects = [];
function makeJsonChartLine (id, ph, valuePh, timestamp, text, label) {
	
	var html = 
		'<div class="activity_text_element">' +
		'	<span class="label label-info">' + timestamp + '</span>&nbsp;' +
		'  	<span class="label label-success">JSON</span>&nbsp;' + 
		' 	<span class="link"><a href="#" onclick="showJson(\''+valuePh+'\', jsonLinkObjects['+id+'].value);">' + text + '</a></span>' +
		'</div>';
	
	ph.append(html);
	
	jsonLinkObjects[id] = {
		value: text,
		timestamp: timestamp	
	};
	
};

function makeTextChart (placeholder, valuePlaceholder, url, label, callback) {

	var ph = $(placeholder);

	jsonGET(url, [], function(dto) {
		
		$(placeholder).empty();
		
		$.each(dto.events, function(key, val) {	
			if (label === 'JSON') {
				makeJsonChartLine(key, ph, valuePlaceholder, val.timestamp, val.value, label);
			} else {
				makeTextChartLine(ph, val.timestamp, val.value, label);	
			}
		});
				
		// if callback exist execute it
		callback && callback();
	});
};

function createChartUrl(url, from, to)  {
	//var newUrl = addParamToUrl(url, 'limit', limit);
	var newUrl = addParamToUrl(url, 'to', to);
	newUrl = addParamToUrl(newUrl, 'from', from);
	return newUrl;
}
 