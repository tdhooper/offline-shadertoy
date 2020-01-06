// https://github.com/rocket/rocket/tree/master/js
var JSRocket = {};
JSRocket.SyncData = function () {

    "use strict";

    var _track = [];

    function getTrack(index) {
        return _track[index];
    }

    function getIndexForName(name) {
        for (var i = 0; i < _track.length; i++) {

            if (_track[i].name === name) {
                return i;
            }
        }

        return -1;
    }

    function getTrackLength() {
        return _track.length;
    }

    function createIndex(varName) {
        var track = new JSRocket.Track();
        track.name = varName;

        _track.push(track);
    }

    return {
        getTrack       :getTrack,
        getIndexForName:getIndexForName,
        getTrackLength :getTrackLength,
        createIndex    :createIndex
    };
};
JSRocket.Track = function () {

    "use strict";

    var STEP = 0,
        LINEAR = 1,
        SMOOTH = 2,
        RAMP = 3;

    var data = [];

    function findKeyIndex(keys, row) {
        var lo = 0, hi = keys.length;
        while (lo < hi) {
            var mi = ((hi + lo) / 2) | 0;

            if (keys[mi] < row) {
                lo = mi + 1;
            } else if (keys[mi] > row) {
                hi = mi;
            } else {
                return mi;
            }
        }
        return lo - 1;
    }

    function getValue(row) {
        var keys = Object.keys(data);

        if (!keys.length) {
            return 0.0;
        }

        var idx = findKeyIndex(keys, Math.floor(row));
        if (idx < 0) {
            return data[keys[0]].value;
        }
        if (idx > keys.length - 2) {
            return data[keys[keys.length - 1]].value;
        }

        // lookup keys and values
        var k0 = keys[idx], k1 = keys[idx + 1];
        var a = data[k0].value;
        var b = data[k1].value;

        // interpolate
        var t = (row - k0) / (k1 - k0);
        switch (data[k0].interpolation) {
        case 0:
          return a;
        case 1:
          return a + (b - a) * t;
        case 2:
          return a + (b - a) * t * t * (3 - 2 * t);
        case 3:
          return a + (b - a) * Math.pow(t, 2.0);
        }
    }

    function add(row, value, interpolation) {
        data[row] = { "value"         :value,
                      "interpolation" :interpolation};
    }

    function remove(row) {
        delete data[row];
    }

    return {
        getValue:getValue,
        add     :add,
        remove  :remove
    };
};

JSRocket.SyncDevicePlayer = function (cfg) {

    "use strict";

    var _urlRequest,
        _syncData = new JSRocket.SyncData(),
        _eventHandler = {
            'ready':function () {
            },
            'error':function () {
            }
        };

    function load(url) {

        _urlRequest = new XMLHttpRequest();

        if (_urlRequest === null) {
            _eventHandler.error();
            return;
        }

        _urlRequest.open('GET', url, true);
        _urlRequest.onreadystatechange = urlRequestHandler;

        _urlRequest.send();
    }

    function urlRequestHandler() {

        if (_urlRequest.readyState === 4) {
            if (_urlRequest.status < 300) {
                readXML(_urlRequest.responseText);
            } else {
                _eventHandler.error();
            }
        }
    }

    function readXML(xmlString) {
        var key,
            t = 0, tLen, k = 0, kLen,
            xml = (new DOMParser()).parseFromString(xmlString, 'text/xml'),
            tracks = xml.getElementsByTagName('tracks');

        //<tracks>
        var trackList = tracks[0].getElementsByTagName('track');

        for (t, tLen = trackList.length; t < tLen; t++) {

            var track = getTrack(trackList[t].getAttribute('name')),
                keyList = trackList[t].getElementsByTagName('key');

            for (k = 0, kLen = keyList.length; k < kLen; k++) {
                key = keyList[k];
                track.add(parseInt(key.getAttribute('row'), 10),
                    parseFloat(key.getAttribute('value')),
                    parseInt(key.getAttribute('interpolation'), 10));

            }
        }

        _eventHandler.ready();
    }

    function getTrack(name) {

        var index = _syncData.getIndexForName(name);

        if (index > -1) {
            return _syncData.getTrack(index);
        }

        _syncData.createIndex(name);
        return _syncData.getTrack(_syncData.getTrackLength() - 1);
    }

    function setEvent(evt, handler) {
        _eventHandler[evt] = handler;
    }

    function nop() {

    }

    if (cfg.rocketXML === "" || cfg.rocketXML === undefined || cfg.rocketXML === undefined) {
        throw("[jsRocket] rocketXML is not set, try _syncDevice.setConfig({'rocketXML':'url/To/RocketXML.rocket'})");
    } else {
        load(cfg.rocketXML);
    }

    return {
        load    :load,
        getTrack:getTrack,
        update  :nop,
        on      :setEvent
    };
};

JSRocket.SyncDeviceClient = function (cfg) {

    "use strict";

    var CMD_SET_KEY = 0,
        CMD_DELETE_KEY = 1,
        CMD_GET_TRACK = 2,
        CMD_SET_ROW = 3,
        CMD_PAUSE = 4,
        CMD_SAVE_TRACKS = 5;

    var _ws = new WebSocket(cfg.socketURL),
        _syncData = new JSRocket.SyncData(),
        _eventHandler = {
            'ready' :function () {
            },
            'update':function () {
            },
            'play'  :function () {
            },
            'pause' :function () {
            },
            'save' :function () {
            }
        };

    function onOpen() {

        _ws.binaryType = "arraybuffer";
        _ws.send('hello, synctracker!');
    }

    function onMessage(e) {

        var queue = (new Uint8Array(e.data)),
            cmd = queue[0],
            track, row, value, interpolation;

        //Handshake
        if (cmd === 104) {

            _eventHandler.ready();

            //PAUSE
        } else if (CMD_PAUSE === cmd) {

            if( queue[1] === 1) {
                _eventHandler.pause();
            } else {
                _eventHandler.play();
            }

            //SET_ROW
        } else if (CMD_SET_ROW === cmd) {

            row = toInt(queue.subarray(1, 5));

            _eventHandler.update(row);

            //SET_KEY
        } else if (CMD_SET_KEY === cmd) {

            track = toInt(queue.subarray(1, 5));
            row = toInt(queue.subarray(5, 9));

            //value = Math.round(toFloat(queue.subarray(9, 13)) * 100) / 100; //round to what's seen in Rocket tracks
            value = toFloat(queue.subarray(9, 13)); //use the values you see in Rocket statusbar

            interpolation = toInt(queue.subarray(13, 14));
            _syncData.getTrack(track).add(row, value, interpolation);

            //DELETE
        } else if (CMD_DELETE_KEY === cmd) {

            track = toInt(queue.subarray(1, 5));
            row = toInt(queue.subarray(5, 9));

            _syncData.getTrack(track).remove(row);

            //SAVE
        } else if (CMD_SAVE_TRACKS === cmd) {
            _eventHandler.save();
        }
    }

    function onClose(e) {
        console.warn(">> connection closed", e);
    }

    function onError(e) {
        console.error(">> connection error'd", e);
    }

    _ws.onopen = onOpen;
    _ws.onmessage = onMessage;
    _ws.onclose = onClose;
    _ws.onerror = onError;

    function getTrack(name) {

        var index = _syncData.getIndexForName(name);

        if (index > -1) {
            return _syncData.getTrack(index);
        }

        var utf8Name = encodeURIComponent(name).replace(/%([\dA-F]{2})/g, function(m, c) {
        return String.fromCharCode('0x' + c);
        });
        var message = [CMD_GET_TRACK,
                        (utf8Name.length >> 24) & 0xFF, (utf8Name.length >> 16) & 0xFF,
                        (utf8Name.length >> 8) & 0xFF, (utf8Name.length ) & 0xFF];

        for (var i = 0; i < utf8Name.length; i++) {
            message.push(utf8Name.charCodeAt(i));
        }

        _ws.send(new Uint8Array(message).buffer);

        _syncData.createIndex(name);
        return _syncData.getTrack(_syncData.getTrackLength() - 1);
    }

    function setRow(row) {

        var streamInt = [(row >> 24) & 0xFF,
                        (row >> 16) & 0xFF,
                        (row >> 8) & 0xFF,
                        (row      ) & 0xFF];

        _ws.send(new Uint8Array([CMD_SET_ROW, streamInt[0], streamInt[1], streamInt[2], streamInt[3]]).buffer);
    }

    function toInt(arr){

        var i = 0,
            view = new DataView(new ArrayBuffer(arr.length));

        for(;i < arr.length; i++) {
            view.setUint8(i, arr[i]);
        }

        if(view.byteLength === 1) {
            return view.getInt8(0);
        } else {
            return view.getInt32(0);
        }
    }

    function toFloat(arr) {
        var view = new DataView(new ArrayBuffer(4));
        view.setUint8(0, arr[0]);
        view.setUint8(1, arr[1]);
        view.setUint8(2, arr[2]);
        view.setUint8(3, arr[3]);

        return view.getFloat32(0);
    }

    function setEvent(evt, handler) {
        _eventHandler[evt] = handler;
    }

    return {
        getTrack:getTrack,
        update  :setRow,
        on      :setEvent
    };
};

JSRocket.SyncDevice = function () {

    "use strict";

    var _connected = false,
        _device,
        _previousIntRow,
        _config = {
            "socketURL":"ws://localhost:1339",
            "rocketXML":""
        },
        _eventHandler = {
            'ready' :function () {
            },
            'update':function () {
            },
            'play'  :function () {
            },
            'pause' :function () {
            }
        };

    function init(mode) {
        if (mode === "demo") {
            _device = new JSRocket.SyncDevicePlayer(_config);
        } else {
            _device = new JSRocket.SyncDeviceClient(_config);
        }

        _device.on('ready', deviceReady);
        _device.on('update', deviceUpdate);
        _device.on('play', devicePlay);
        _device.on('pause', devicePause);
    }

    function getConfig() {
        return _config;
    }

    function setConfig(cfg) {
        for (var option in cfg) {
            if (cfg.hasOwnProperty(option)) {
                _config[option] = cfg[option];
            }
        }

        return _config;
    }

    function deviceReady() {
        _connected = true;
        _eventHandler.ready();
    }

    function deviceUpdate(row) {
        _eventHandler.update(row);
    }

    function devicePlay() {
        _eventHandler.play();
    }

    function devicePause() {
        _eventHandler.pause();
    }

    function getTrack(name) {
        if (_connected) {
            return _device.getTrack(name);
        } else {
            return null;
        }
    }

    function update(row) {
        //no need to update rocket on float rows
        if (Math.floor(row) !== _previousIntRow) {
            _previousIntRow = Math.floor(row);
            _device.update(_previousIntRow);
        }
    }

    function setEvent(evt, handler) {
        _eventHandler[evt] = handler;
    }

    return {
        init     :init,
        setConfig:setConfig,
        getConfig:getConfig,
        getTrack :getTrack,
        update   :update,
        on       :setEvent
    };
};

module.exports = JSRocket;
