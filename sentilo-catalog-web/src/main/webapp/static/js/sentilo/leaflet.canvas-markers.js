/******/ (() => { // webpackBootstrap
/******/ 	var __webpack_modules__ = ([
/* 0 */,
/* 1 */
/***/ (function(module) {

!function(t,i){ true?module.exports=i():0}(this,function(){"use strict";function t(t,r,e,a,h){!function t(n,r,e,a,h){for(;a>e;){if(a-e>600){var o=a-e+1,s=r-e+1,l=Math.log(o),f=.5*Math.exp(2*l/3),u=.5*Math.sqrt(l*f*(o-f)/o)*(s-o/2<0?-1:1),m=Math.max(e,Math.floor(r-s*f/o+u)),c=Math.min(a,Math.floor(r+(o-s)*f/o+u));t(n,r,m,c,h)}var p=n[r],d=e,x=a;for(i(n,e,r),h(n[a],p)>0&&i(n,e,a);d<x;){for(i(n,d,x),d++,x--;h(n[d],p)<0;)d++;for(;h(n[x],p)>0;)x--}0===h(n[e],p)?i(n,e,x):i(n,++x,a),x<=r&&(e=x+1),r<=x&&(a=x-1)}}(t,r,e||0,a||t.length-1,h||n)}function i(t,i,n){var r=t[i];t[i]=t[n],t[n]=r}function n(t,i){return t<i?-1:t>i?1:0}var r=function(t){void 0===t&&(t=9),this._maxEntries=Math.max(4,t),this._minEntries=Math.max(2,Math.ceil(.4*this._maxEntries)),this.clear()};function e(t,i,n){if(!n)return i.indexOf(t);for(var r=0;r<i.length;r++)if(n(t,i[r]))return r;return-1}function a(t,i){h(t,0,t.children.length,i,t)}function h(t,i,n,r,e){e||(e=p(null)),e.minX=1/0,e.minY=1/0,e.maxX=-1/0,e.maxY=-1/0;for(var a=i;a<n;a++){var h=t.children[a];o(e,t.leaf?r(h):h)}return e}function o(t,i){return t.minX=Math.min(t.minX,i.minX),t.minY=Math.min(t.minY,i.minY),t.maxX=Math.max(t.maxX,i.maxX),t.maxY=Math.max(t.maxY,i.maxY),t}function s(t,i){return t.minX-i.minX}function l(t,i){return t.minY-i.minY}function f(t){return(t.maxX-t.minX)*(t.maxY-t.minY)}function u(t){return t.maxX-t.minX+(t.maxY-t.minY)}function m(t,i){return t.minX<=i.minX&&t.minY<=i.minY&&i.maxX<=t.maxX&&i.maxY<=t.maxY}function c(t,i){return i.minX<=t.maxX&&i.minY<=t.maxY&&i.maxX>=t.minX&&i.maxY>=t.minY}function p(t){return{children:t,height:1,leaf:!0,minX:1/0,minY:1/0,maxX:-1/0,maxY:-1/0}}function d(i,n,r,e,a){for(var h=[n,r];h.length;)if(!((r=h.pop())-(n=h.pop())<=e)){var o=n+Math.ceil((r-n)/e/2)*e;t(i,o,n,r,a),h.push(n,o,o,r)}}return r.prototype.all=function(){return this._all(this.data,[])},r.prototype.search=function(t){var i=this.data,n=[];if(!c(t,i))return n;for(var r=this.toBBox,e=[];i;){for(var a=0;a<i.children.length;a++){var h=i.children[a],o=i.leaf?r(h):h;c(t,o)&&(i.leaf?n.push(h):m(t,o)?this._all(h,n):e.push(h))}i=e.pop()}return n},r.prototype.collides=function(t){var i=this.data;if(!c(t,i))return!1;for(var n=[];i;){for(var r=0;r<i.children.length;r++){var e=i.children[r],a=i.leaf?this.toBBox(e):e;if(c(t,a)){if(i.leaf||m(t,a))return!0;n.push(e)}}i=n.pop()}return!1},r.prototype.load=function(t){if(!t||!t.length)return this;if(t.length<this._minEntries){for(var i=0;i<t.length;i++)this.insert(t[i]);return this}var n=this._build(t.slice(),0,t.length-1,0);if(this.data.children.length)if(this.data.height===n.height)this._splitRoot(this.data,n);else{if(this.data.height<n.height){var r=this.data;this.data=n,n=r}this._insert(n,this.data.height-n.height-1,!0)}else this.data=n;return this},r.prototype.insert=function(t){return t&&this._insert(t,this.data.height-1),this},r.prototype.clear=function(){return this.data=p([]),this},r.prototype.remove=function(t,i){if(!t)return this;for(var n,r,a,h=this.data,o=this.toBBox(t),s=[],l=[];h||s.length;){if(h||(h=s.pop(),r=s[s.length-1],n=l.pop(),a=!0),h.leaf){var f=e(t,h.children,i);if(-1!==f)return h.children.splice(f,1),s.push(h),this._condense(s),this}a||h.leaf||!m(h,o)?r?(n++,h=r.children[n],a=!1):h=null:(s.push(h),l.push(n),n=0,r=h,h=h.children[0])}return this},r.prototype.toBBox=function(t){return t},r.prototype.compareMinX=function(t,i){return t.minX-i.minX},r.prototype.compareMinY=function(t,i){return t.minY-i.minY},r.prototype.toJSON=function(){return this.data},r.prototype.fromJSON=function(t){return this.data=t,this},r.prototype._all=function(t,i){for(var n=[];t;)t.leaf?i.push.apply(i,t.children):n.push.apply(n,t.children),t=n.pop();return i},r.prototype._build=function(t,i,n,r){var e,h=n-i+1,o=this._maxEntries;if(h<=o)return a(e=p(t.slice(i,n+1)),this.toBBox),e;r||(r=Math.ceil(Math.log(h)/Math.log(o)),o=Math.ceil(h/Math.pow(o,r-1))),(e=p([])).leaf=!1,e.height=r;var s=Math.ceil(h/o),l=s*Math.ceil(Math.sqrt(o));d(t,i,n,l,this.compareMinX);for(var f=i;f<=n;f+=l){var u=Math.min(f+l-1,n);d(t,f,u,s,this.compareMinY);for(var m=f;m<=u;m+=s){var c=Math.min(m+s-1,u);e.children.push(this._build(t,m,c,r-1))}}return a(e,this.toBBox),e},r.prototype._chooseSubtree=function(t,i,n,r){for(;r.push(i),!i.leaf&&r.length-1!==n;){for(var e=1/0,a=1/0,h=void 0,o=0;o<i.children.length;o++){var s=i.children[o],l=f(s),u=(m=t,c=s,(Math.max(c.maxX,m.maxX)-Math.min(c.minX,m.minX))*(Math.max(c.maxY,m.maxY)-Math.min(c.minY,m.minY))-l);u<a?(a=u,e=l<e?l:e,h=s):u===a&&l<e&&(e=l,h=s)}i=h||i.children[0]}var m,c;return i},r.prototype._insert=function(t,i,n){var r=n?t:this.toBBox(t),e=[],a=this._chooseSubtree(r,this.data,i,e);for(a.children.push(t),o(a,r);i>=0&&e[i].children.length>this._maxEntries;)this._split(e,i),i--;this._adjustParentBBoxes(r,e,i)},r.prototype._split=function(t,i){var n=t[i],r=n.children.length,e=this._minEntries;this._chooseSplitAxis(n,e,r);var h=this._chooseSplitIndex(n,e,r),o=p(n.children.splice(h,n.children.length-h));o.height=n.height,o.leaf=n.leaf,a(n,this.toBBox),a(o,this.toBBox),i?t[i-1].children.push(o):this._splitRoot(n,o)},r.prototype._splitRoot=function(t,i){this.data=p([t,i]),this.data.height=t.height+1,this.data.leaf=!1,a(this.data,this.toBBox)},r.prototype._chooseSplitIndex=function(t,i,n){for(var r,e,a,o,s,l,u,m=1/0,c=1/0,p=i;p<=n-i;p++){var d=h(t,0,p,this.toBBox),x=h(t,p,n,this.toBBox),v=(e=d,a=x,o=void 0,s=void 0,l=void 0,u=void 0,o=Math.max(e.minX,a.minX),s=Math.max(e.minY,a.minY),l=Math.min(e.maxX,a.maxX),u=Math.min(e.maxY,a.maxY),Math.max(0,l-o)*Math.max(0,u-s)),M=f(d)+f(x);v<m?(m=v,r=p,c=M<c?M:c):v===m&&M<c&&(c=M,r=p)}return r||n-i},r.prototype._chooseSplitAxis=function(t,i,n){var r=t.leaf?this.compareMinX:s,e=t.leaf?this.compareMinY:l;this._allDistMargin(t,i,n,r)<this._allDistMargin(t,i,n,e)&&t.children.sort(r)},r.prototype._allDistMargin=function(t,i,n,r){t.children.sort(r);for(var e=this.toBBox,a=h(t,0,i,e),s=h(t,n-i,n,e),l=u(a)+u(s),f=i;f<n-i;f++){var m=t.children[f];o(a,t.leaf?e(m):m),l+=u(a)}for(var c=n-i-1;c>=i;c--){var p=t.children[c];o(s,t.leaf?e(p):p),l+=u(s)}return l},r.prototype._adjustParentBBoxes=function(t,i,n){for(var r=n;r>=0;r--)o(i[r],t)},r.prototype._condense=function(t){for(var i=t.length-1,n=void 0;i>=0;i--)0===t[i].children.length?i>0?(n=t[i-1].children).splice(n.indexOf(t[i]),1):this.clear():a(t[i],this.toBBox)},r});


/***/ }),
/* 2 */
/***/ ((module) => {

"use strict";


function layerFactory(L) {

    const CanvasIconLayer = (L.Layer ? L.Layer : L.Class).extend({

        //Add event listeners to initialized section.
        initialize: function (options) {

            L.setOptions(this, options);
            this._onClickListeners = [];
            this._onHoverListeners = [];

            // Spider
            this.twoPi = Math.PI * 2;
            this._markersArray = [];
            this.listeners = {};
            this.circleFootSeparation = 25
            this.keepSpiderfied = false;        // yes -> don't unspiderfy when a marker is selected
            this.nearbyDistance = 20;           // spiderfy markers within this range of the one clicked, in px

            this.circleSpiralSwitchover = 9;    // show spiral instead of circle from this marker count upwards
            // 0 -> always spiral; Infinity -> always circle
            this.circleFootSeparation = 25;     // related to circumference of circle
            this.circleStartAngle = this.twoPi / 12;
            this.spiralFootSeparation = 28;     // related to size of spiral (experiment!)
            this.spiralLengthStart = 11;        // ditto
            this.spiralLengthFactor = 5;        // ditto

            this.legWeight = 1.5;
            this.legColors = {
                'usual': '#222',
                'highlighted': '#f00'
            };
        },

        setOptions: function (options) {

            L.setOptions(this, options);
            return this.redraw();
        },

        redraw: function () {

            this._redraw(true);
        },

        //Multiple layers at a time for rBush performance
        addMarkers: function (markers) {

            var self = this;
            var tmpMark = [];
            var tmpLatLng = [];

            self._markersArray.push(...markers);

            markers.forEach(function (marker) {

                if (!((marker.options.pane == 'markerPane') && marker.options.icon)) {
                    console.error('Layer isn\'t a marker');
                    return;
                }

                var latlng = marker.getLatLng();
                var isDisplaying = self._map.getBounds().contains(latlng);
                var s = self._addMarker(marker, latlng, isDisplaying);

                //Only add to Point Lookup if we are on map
                if (isDisplaying === true) tmpMark.push(s[0]);

                tmpLatLng.push(s[1]);
            });

            self._markers.load(tmpMark);
            self._latlngMarkers.load(tmpLatLng);
        },

        //Adds single layer at a time. Less efficient for rBush
        addMarker: function (marker) {
            const self = this;

            const markerListener = () => {                
                this.spiderListener(marker);
            }

            marker.addEventListener('click', markerListener);

            const coordinate = marker.getLatLng();
            const isDisplaying = self._map.getBounds().contains(coordinate);
            const dat = self._addMarker(marker, coordinate, isDisplaying);

            self._markersArray.push(marker);

            //Only add to Point Lookup if we are on map
            if (isDisplaying === true) self._markers.insert(dat[0]);

            self._latlngMarkers.insert(dat[1]);

        },

        addLayer: function (layer) {
            if ((layer.options.pane !== 'markerPane')) {
                console.log(layer);
            }
            if ((layer.options.pane === 'markerPane') && layer.options.icon) this.addMarker(layer);
            else console.error('Layer isn\'t a marker');
        },

        addLayers: function (layers) {
            this.addMarkers(layers);
        },

        removeLayer: function (layer) {
            this.removeMarker(layer, true);
        },

        removeMarker: function (marker, redraw) {

            var self = this;

            //If we are removed point
            if (marker["minX"]) marker = marker.data;

            var latlng = marker.getLatLng();
            var isDisplaying = self._map.getBounds().contains(latlng);

            var markerData = {

                minX: latlng.lng,
                minY: latlng.lat,
                maxX: latlng.lng,
                maxY: latlng.lat,
                data: marker
            };

            self._latlngMarkers.remove(markerData, function (a, b) {

                return a.data._leaflet_id === b.data._leaflet_id;
            });

            const index = this._markersArray.indexOf(marker);

            if (index > -1) {
                this._markersArray.splice(index, 1);
            }

            self._latlngMarkers.total--;
            self._latlngMarkers.dirty++;

            if (isDisplaying === true && redraw === true) {
                self._redraw(true);
            }
        },

        onAdd: function (map) {

            this._map = map;

            if (!this._canvas) this._initCanvas();

            if (this.options.pane) this.getPane().appendChild(this._canvas);
            else map._panes.overlayPane.appendChild(this._canvas);

            map.on('moveend', this._reset, this);
            map.on('resize', this._reset, this);

            map.on('click', this._executeListeners, this);
            map.on('mousemove', this._executeListeners, this);
        },

        onRemove: function (map) {

            if (this.options.pane) this.getPane().removeChild(this._canvas);
            else map.getPanes().overlayPane.removeChild(this._canvas);

            map.off('click', this._executeListeners, this);
            map.off('mousemove', this._executeListeners, this);

            map.off('moveend', this._reset, this);
            map.off('resize', this._reset, this);
        },

        addTo: function (map) {

            map.addLayer(this);
            return this;
        },

        clearLayers: function (redraw= true) {
            this._latlngMarkers = null;
            this._markers = null;
            this._markersArray = [];
            this._redraw(redraw);
        },

        spiderfy: function (marker) {
            const nearbyMarkerData = [];
            const nonNearbyMarkers = [];
            const nearbyDistance = 20;
            const pxSq = nearbyDistance * nearbyDistance;

            const markerPt = map.latLngToLayerPoint(marker.getLatLng());

            for (let m of this._markersArray) {
                const mPt = map.latLngToLayerPoint(m.getLatLng());

                if (canvasLayer.ptDistanceSq(mPt, markerPt) < pxSq) {
                    nearbyMarkerData.push({marker: m, markerPt: mPt});
                } else {
                    nonNearbyMarkers.push(m);
                }
            }
            if (nearbyMarkerData.length === 1) {  // 1 => the one clicked => none nearby
                this._trigger('popup', marker);
            } else {
                const options = marker.options;
                if (options && options.spiderfy === false ) {
                    return this;
                }
                return this._spiderfy(nearbyMarkerData, nonNearbyMarkers);
            }
        },

        _spiderfy: function (markerData, nonNearbyMarkers) {
            let md;
            this.spiderfying = true;
            const numFeet = markerData.length;
            const bodyPt = this.ptAverage((() => {
                const result = [];
                for (md of Array.from(markerData)) {
                    result.push(md.markerPt);
                }
                return result;
            })());
            const footPts = numFeet >= this.circleSpiralSwitchover ?
                this._generatePtsSpiral(numFeet, bodyPt).reverse()  // match from outside in => less cross-crossing
                :
                this._generatePtsCircle(numFeet, bodyPt);
            const spiderfiedMarkers = (() => {
                const result = [];
                for (var footPt of Array.from(footPts)) {
                    const footLl = map.layerPointToLatLng(footPt);
                    const nearestMarkerDatum = this._minExtract(markerData, md => this._ptDistanceSq(md.markerPt, footPt));
                    const {marker} = nearestMarkerDatum;
                    const leg = new L.Polyline([marker.getLatLng(), footLl], {
                        color: this.legColors.usual,
                        weight: this.legWeight,
                        clickable: false
                    });
                    //map.addLayer(leg);
                    marker.omsData = {usualPosition: marker.getLatLng(), leg};
                    if (this.legColors.highlighted !== this.legColors.usual) {
                        const mhl = this._makeHighlightListeners(marker);
                        marker.omsData.highlightListeners = mhl;
                        marker.addEventListener('mouseout', mhl.unhighlight);
                    }
                    this.removeLayer(marker);
                    marker.setLatLng(footLl);
                    this.addMarker(marker);
                    result.push(marker);
                }
                return result;
            })();
            delete this.spiderfying;
            this.spiderfied = true;
            return this._trigger('spiderfy', spiderfiedMarkers, nonNearbyMarkers);
        },

        unspiderfy: function (markerNotToMove = null) {
            if (this.spiderfied == null) {
                return this;
            }
            this.unspiderfying = true;
            const unspiderfiedMarkers = [];
            const nonNearbyMarkers = [];
            for (let marker of Array.from(this._markersArray)) {
                if (marker.omsData != null) {
                    map.removeLayer(marker.omsData.leg);
                    if (marker !== markerNotToMove) {
                        this.removeLayer(marker);
                        marker.setLatLng(marker.omsData.usualPosition);
                        this.addMarker(marker);
                    }
                    marker.setZIndexOffset(0);
                    const mhl = marker.omsData.highlightListeners;
                    if (mhl != null) {
                        marker.removeEventListener('mouseover', mhl.highlight);
                        marker.removeEventListener('mouseout', mhl.unhighlight);
                    }
                    delete marker.omsData;
                    unspiderfiedMarkers.push(marker);
                } else {
                    nonNearbyMarkers.push(marker);
                }
            }
            delete this.unspiderfying;
            delete this.spiderfied;
            this._trigger('unspiderfy', unspiderfiedMarkers, nonNearbyMarkers);
            return this;  // return self, for chaining
        },

        _closePopup: function (marker) {
            marker.closePopup();
        },

        spiderListener: function (marker) {
            const markerSpiderfied = (marker.omsData != null);
            if (!markerSpiderfied || !this.keepSpiderfied) {
                this.unspiderfy()
            }
            if (markerSpiderfied) {
                return this._trigger('click', marker);
            } else {
                const nearbyMarkerData = [];
                const nonNearbyMarkers = [];
                const pxSq = this.nearbyDistance * this.nearbyDistance;
                const markerPt = map.latLngToLayerPoint(marker.getLatLng());
                for (let m of Array.from(this._markersArray)) {
                    /*if (!map.hasLayer(m)) {
                        continue;
                    }*/
                    const mPt = map.latLngToLayerPoint(m.getLatLng());
                    if (this.ptDistanceSq(mPt, markerPt) < pxSq) {
                        nearbyMarkerData.push({marker: m, markerPt: mPt});
                    } else {
                        nonNearbyMarkers.push(m);
                    }
                }
                if (nearbyMarkerData.length === 0) {  // 0 => one spidered marker clicked => none nearby
                    return this._trigger('click', marker);
                } else if (nearbyMarkerData.length === 1) {  // 1 => the one clicked => none nearby
                    return this._trigger('click', marker);
                } else {
                    this._closePopup(marker);
                    return this._spiderfy(nearbyMarkerData, nonNearbyMarkers);
                }
            }
        },

        addListener: function (event, func) {
            (this.listeners[event] != null ? this.listeners[event] : (this.listeners[event] = [])).push(func);
            return this;  // return self, for chaining
        },

        _generatePtsSpiral: function (count, centerPt) {
            let legLength = this.spiralLengthStart;
            let angle = 0;
            return (() => {
                const result = [];
                for (let i = 0, end = count, asc = 0 <= end; asc ? i < end : i > end; asc ? i++ : i--) {
                    angle += (this.spiralFootSeparation / legLength) + (i * 0.0005);
                    const pt = new L.Point(centerPt.x + (legLength * Math.cos(angle)), centerPt.y + (legLength * Math.sin(angle)));
                    legLength += (this.twoPi * this.spiralLengthFactor) / angle;
                    result.push(pt);
                }
                return result;
            })();
        },

        _makeHighlightListeners: function (marker) {
            return {
                highlight: () => marker.omsData.leg.setStyle({color: this.legColors.highlighted}).bringToBack(),
                unhighlight: () => marker.omsData.leg.setStyle({color: this.legColors.usual}).bringToBack()
            };
        },

        _generatePtsCircle: function (count, centerPt) {
            const twoPi = Math.PI * 2;
            const circumference = this.circleFootSeparation * (2 + count);
            const legLength = circumference / twoPi;  // = radius from circumference
            const angleStep = twoPi / count;
            return (() => {
                const result = [];
                for (let i = 0, end = count, asc = 0 <= end; asc ? i < end : i > end; asc ? i++ : i--) {
                    const angle = this.circleStartAngle + (i * angleStep);
                    result.push(new L.Point(centerPt.x + (legLength * Math.cos(angle)),
                        centerPt.y + (legLength * Math.sin(angle))));
                }
                return result;
            })();
        },

        _ptDistanceSq: function (pt1, pt2) {
            const dx = pt1.x - pt2.x;
            const dy = pt1.y - pt2.y;
            return (dx * dx) + (dy * dy);
        },

        _ptAverage: function (pts) {
            let sumY;
            let sumX = (sumY = 0);
            for (let pt of Array.from(pts)) {
                sumX += pt.x;
                sumY += pt.y;
            }
            const numPts = pts.length;
            return new L.Point(sumX / numPts, sumY / numPts);
        },

        _minExtract: function (set, func) {  // destructive! returns minimum, and also removes it from the set
            let bestIndex;
            for (let index = 0; index < set.length; index++) {
                const item = set[index];
                const val = func(item);
                if ((bestIndex == null) || (val < bestVal)) {
                    var bestVal = val;
                    bestIndex = index;
                }
            }
            return set.splice(bestIndex, 1)[0];
        },

        _addMarker: function (marker, latlng, isDisplaying) {

            var self = this;
            //Needed for pop-up & tooltip to work.
            marker._map = self._map;

            //_markers contains Points of markers currently displaying on map
            if (!self._markers) self._markers = new rbush();

            //_latlngMarkers contains Lat\Long coordinates of all markers in layer.
            if (!self._latlngMarkers) {
                self._latlngMarkers = new rbush();
                self._latlngMarkers.dirty = 0;
                self._latlngMarkers.total = 0;
            }

            L.Util.stamp(marker);

            var pointPos = self._map.latLngToContainerPoint(latlng);
            var iconSize = marker.options.icon.options.iconSize;

            var adj_x = iconSize[0] / 2;
            var adj_y = iconSize[1] / 2;
            var ret = [({
                minX: (pointPos.x - adj_x),
                minY: (pointPos.y - adj_y),
                maxX: (pointPos.x + adj_x),
                maxY: (pointPos.y + adj_y),
                data: marker
            }), ({
                minX: latlng.lng,
                minY: latlng.lat,
                maxX: latlng.lng,
                maxY: latlng.lat,
                data: marker
            })];

            self._latlngMarkers.dirty++;
            self._latlngMarkers.total++;

            //Only draw if we are on map
            if (isDisplaying === true) self._drawMarker(marker, pointPos);

            return ret;
        },

        _drawMarker: function (marker, pointPos) {

            var self = this;

            if (!this._imageLookup) this._imageLookup = {};
            if (!pointPos) {

                pointPos = self._map.latLngToContainerPoint(marker.getLatLng());
            }

            var iconUrl = marker.options.icon.options.iconUrl;

            if (marker.canvas_img) {

                self._drawImage(marker, pointPos);
            } else {

                if (self._imageLookup[iconUrl]) {

                    marker.canvas_img = self._imageLookup[iconUrl][0];

                    if (self._imageLookup[iconUrl][1] === false) {

                        self._imageLookup[iconUrl][2].push([marker, pointPos]);
                    } else {

                        self._drawImage(marker, pointPos);
                    }
                } else {

                    var i = new Image();
                    i.src = iconUrl;
                    marker.canvas_img = i;

                    //Image,isLoaded,marker\pointPos ref
                    self._imageLookup[iconUrl] = [i, false, [[marker, pointPos]]];

                    i.onload = function () {

                        self._imageLookup[iconUrl][1] = true;
                        self._imageLookup[iconUrl][2].forEach(function (e) {

                            self._drawImage(e[0], e[1]);
                        });
                    }
                }
            }
        },

        _trigger: function (event, ...args) {
            return (Array.from(this.listeners[event] != null ? this.listeners[event] : [])).map((func) => func(...Array.from(args || [])));
        },

        _drawImage: function (marker, pointPos) {

            const options = marker.options.icon.options;

            this._context.drawImage(
                marker.canvas_img,
                pointPos.x - options.iconAnchor[0],
                pointPos.y - options.iconAnchor[1],
                options.iconSize[0],
                options.iconSize[1]
            );
        },

        _reset: function () {

            var topLeft = this._map.containerPointToLayerPoint([0, 0]);
            L.DomUtil.setPosition(this._canvas, topLeft);

            var size = this._map.getSize();

            this._canvas.width = size.x;
            this._canvas.height = size.y;

            this._redraw();
        },

        _redraw: function (clear) {

            var self = this;

            if (clear) this._context.clearRect(0, 0, this._canvas.width, this._canvas.height);
            if (!this._map || !this._latlngMarkers) return;

            var tmp = [];

            //If we are 10% individual inserts\removals, reconstruct lookup for efficiency
            if (self._latlngMarkers.dirty / self._latlngMarkers.total >= .1) {

                self._latlngMarkers.all().forEach(function (e) {

                    tmp.push(e);
                });

                self._latlngMarkers.clear();
                self._latlngMarkers.load(tmp);
                self._latlngMarkers.dirty = 0;
                tmp = [];
            }

            var mapBounds = self._map.getBounds();

            //Only re-draw what we are showing on the map.

            var mapBoxCoords = {
                minX: mapBounds.getWest(),
                minY: mapBounds.getSouth(),
                maxX: mapBounds.getEast(),
                maxY: mapBounds.getNorth(),
            };

            self._latlngMarkers.search(mapBoxCoords).forEach(function (e) {

                //Readjust Point Map
                var pointPos = self._map.latLngToContainerPoint(e.data.getLatLng());

                var iconSize = e.data.options.icon.options.iconSize;
                var adj_x = iconSize[0] / 2;
                var adj_y = iconSize[1] / 2;

                var newCoords = {
                    minX: (pointPos.x - adj_x),
                    minY: (pointPos.y - adj_y),
                    maxX: (pointPos.x + adj_x),
                    maxY: (pointPos.y + adj_y),
                    data: e.data
                }

                tmp.push(newCoords);

                //Redraw points
                self._drawMarker(e.data, pointPos);
            });

            //Clear rBush & Bulk Load for performance
            this._markers.clear();
            this._markers.load(tmp);
        },

        _initCanvas: function () {

            this._canvas = L.DomUtil.create('canvas', 'leaflet-canvas-icon-layer leaflet-layer');
            var originProp = L.DomUtil.testProp(['transformOrigin', 'WebkitTransformOrigin', 'msTransformOrigin']);
            this._canvas.style[originProp] = '50% 50%';

            var size = this._map.getSize();
            this._canvas.width = size.x;
            this._canvas.height = size.y;

            this._context = this._canvas.getContext('2d');

            var animated = this._map.options.zoomAnimation && L.Browser.any3d;
            L.DomUtil.addClass(this._canvas, 'leaflet-zoom-' + (animated ? 'animated' : 'hide'));
        },

        addOnClickListener: function (listener) {
            this._onClickListeners.push(listener);
        },

        addOnHoverListener: function (listener) {
            this._onHoverListeners.push(listener);
        },

        _executeListeners: function (event) {

            if (!this._markers) return;

            var me = this;
            var x = event.containerPoint.x;
            var y = event.containerPoint.y;

            if (me._openToolTip) {

                me._openToolTip.closeTooltip();
                delete me._openToolTip;
            }

            var ret = this._markers.search({minX: x, minY: y, maxX: x, maxY: y});

            if (ret && ret.length > 0) {

                me._map._container.style.cursor = "pointer";

                if (event.type === "click") {

                    /*const hasPopup = ret[0].data.getPopup();
                    if (hasPopup) ret[0].data.openPopup();*/

                    me._onClickListeners.forEach(function (listener) {
                        listener(event, ret);
                    });
                }

                if (event.type === "mousemove") {
                    var hasTooltip = ret[0].data.getTooltip();
                    if (hasTooltip) {
                        me._openToolTip = ret[0].data;
                        ret[0].data.openTooltip();
                    }

                    me._onHoverListeners.forEach(function (listener) {
                        listener(event, ret);
                    });
                }
            } else {

                me._map._container.style.cursor = "";
            }
        },

        ptAverage: function (pts) {
            let sumY;
            let sumX = (sumY = 0);
            for (let pt of Array.from(pts)) {
                sumX += pt.x;
                sumY += pt.y;
            }
            const numPts = pts.length;
            return new L.Point(sumX / numPts, sumY / numPts);
        },

        ptDistanceSq: function (pt1, pt2) {
            const dx = pt1.x - pt2.x;
            const dy = pt1.y - pt2.y;
            return (dx * dx) + (dy * dy);
        }
    });

    L.canvasIconLayer = function (options) {
        return new CanvasIconLayer(options);
    };
}

module.exports = layerFactory;


/***/ })
/******/ 	]);
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		if(__webpack_module_cache__[moduleId]) {
/******/ 			return __webpack_module_cache__[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
(() => {
var rbush = __webpack_require__(1);
var factory = __webpack_require__(2);

window.L.CanvasIconLayer = factory(L);
window.rbush = rbush;

})();

/******/ })()
;