"use strict"

// module Node.IRC

exports.channelMessageFromArgumentsJS = function (args) {
  return {
    nick: args[0],
    text: args[1],
  }
}

exports.privateMessageFromArgumentsJS = function (args) {
  return {
    nick: args[0],
    to: args[1],
    text: args[2]
  }
}

exports.inspect = require('util').inspect
