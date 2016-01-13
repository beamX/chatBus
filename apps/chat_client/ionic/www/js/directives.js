'use strict';

/* Controllers */

var cbc = angular.module('cbc');

cbc
  .directive('msgCard', function() {
    return {
      restrict: 'E',
      scope: {
        message: "="
      },
      templateUrl: './js/tpl/msg-card.htm',
      link : function(scope, elm, attrs) {
        scope.initials = scope.message.sender.charAt(0).toUpperCase();
      }
    };
  })

  .directive('hitchHikers', function() {
    return {
      restrict: 'E',
      templateUrl: './js/tpl/hitch-hikers.htm'
    };
  })

  .directive('channels', function() {
    return {
      restrict: 'E',
      templateUrl: './js/tpl/channels.htm'
    };
  })
