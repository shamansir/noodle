"use strict";

exports.collectLinksPositions = function(links) {
    return function() {
        console.log('collect', links);
        return [];
    };
}
