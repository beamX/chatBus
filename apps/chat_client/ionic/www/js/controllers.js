'use strict';

/* Controllers */

var cbc = angular.module('cbc');

cbc
  .controller('TodoCtrl', function($scope, $timeout, $ionicModal, $ionicSideMenuDelegate, ws) {

    $scope.hitchhikers   = ws.hikers_list;
    $scope.channels      = ws.bus_list;
    $scope.activeChannel = ws.activeChannel;
    $scope.username      = ws.username;
    $scope.messages      = ws.messages;

    // handle toogle
    $scope.toggleChannels = function() {
      $ionicSideMenuDelegate.toggleLeft();
    };

    // Called to create a new bus
    $scope.newChannel = function() {
      var Channel = prompt('Enter Bus name');
      if(Channel) {
        ws.add_bus(Channel);
      }
    };

    // Called to select the given channel
    $scope.selectChannel = function(channel, index) {
      ws.change_bus(channel);
      ws.get_hikers();
      // $scope.activeChannel = channel;
      $ionicSideMenuDelegate.toggleLeft(false);
    };

    $scope.message = "";
    $scope.send_message = function(message) {
      console.log($scope.message);
      ws.send_chat($scope.message);
      $scope.message = "";

    };

  });
