"use strict";

// module Main

exports.asBody = function(content){
    return function(){
        document.body.innerHTML = content;
    }
}
