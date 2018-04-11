"use strict";

exports.onPopStateImpl = function onPopStateImpl(f,w) {
  w.onpopstate = function onPopStateImplHandler(event) {
    f(event.state);
  };
};


exports.queryParams = function queryParamsImpl(loc) {
  var match,
    pl     = /\+/g,  // Regex for replacing addition symbol with a space
    search = /([^&=]+)=?([^&]*)/g,
    decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
    query  = loc.search.substring(1),
    result = {};
  while (match = search.exec(query))
    result[decode(match[1])] = decode(match[2]);

  return result;
}


exports.removeQueryParamImpl = function removeQueryParamImpl(loc, parameter) {
    // ripped from https://stackoverflow.com/questions/1634748/how-can-i-delete-a-query-string-parameter-in-javascript
    function removeURLParameter(url) {
        //prefer to use l.search if you have a location/link object
        var urlparts= url.split('?');
        if (urlparts.length>=2) {

            var prefix= encodeURIComponent(parameter)+'=';
            var pars= urlparts[1].split(/[&;]/g);

            //reverse iteration as may be destructive
            for (var i= pars.length; i-- > 0;) {
                //idiom for string.startsWith
                if (pars[i].lastIndexOf(prefix, 0) !== -1) {
                    pars.splice(i, 1);
                }
            }

            url= urlparts[0] + (pars.length > 0 ? '?' + pars.join('&') : "");
            return url;
        } else {
            return url;
        }
    }

    var q = loc.search;
    loc.search = removeURLParameter(q);
}
