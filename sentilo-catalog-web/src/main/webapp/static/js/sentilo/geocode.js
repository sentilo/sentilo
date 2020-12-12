function Geocode() {
    this.geocode = function (geocodeProps, callback) {
		if(geocodeProps.address){
			search(geocodeProps.address, callback);
		}else if(geocodeProps.latLng){
			reverse(geocodeProps.latLng, callback);
		}
	}	            
	
	function reverse(location, result) {
        const queryData = {
            lat: location.lat,
            lon: location.lng,
            format: 'json'
        }
        jsonGET('https://nominatim.openstreetmap.org/reverse', queryData, result);
	}
	
	function search(address, result) {
		const queryData = {
            q: address,
            format: 'json'
        }
        jsonGET('https://nominatim.openstreetmap.org/search', queryData, result);
	}
}
