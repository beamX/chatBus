'use strict';

/* Factory and services */

var cbc = angular.module('cbc');

cbc
  .factory('ws', function($websocket) {
    var ws_url = "ws://" + location.host + "/ws";
    var ws     = $websocket(ws_url);
    var bus_list = [];
    var hikers_list = [];
    var username = "";
    var activeChannel = "";
    var messages = [
      {sender : 'ChatBus', text: 'Hello world!'},
    ];

    ws.onError(function(event) {
      console.log('connection Error', event);
    });

    ws.onClose(function(event) {
      send_msg('terminate', "");
      console.log('connection closed', event);
    });

    ws.onOpen(function() {
      console.log('connection open');
      get_bus_list();
    });

    ws.onMessage(function(e) {
      var Data = JSON.parse(e.data);
      console.log("got data", Data);

      if (Data.type == 'bus_list'){
        bus_list = Data.msg;
      } else if (Data.type == 'bus_subscribed') {
        activeChannel = Data.msg;
      } else if (Data.type == 'hitchhicker_list') {
        hikers_list = Data.msg;
      } else if (Data.type == 'chat') {
        messages.push(Data.msg);
      } else if (Data.type == 'username') {
        username = Data.msg;
      } else if (Data.type == 'username_error') {
        alert("Username already exists !");
      }
      else{
        console.log("unkown message: ", Data);
      }

    });

    function choose_username() {
      if(username == "") {
        var uname = prompt('Choose username:');
        if(uname) {
          set_username(uname);
        }
      }
    };

    function send_msg(mType, mValue) {
      if (mType == "chat" && username == "") {return choose_username();};
      if (mType == "chat") { messages.push(mValue)};

      var Message = {
        type: mType,
        msg : mValue
      };
      console.log("sending ", Message);
      ws.send(JSON.stringify(Message));

    };

    // set the username for the current session
    var set_username = function(name) {
      send_msg('username', name);
    };

    // set the username for the current session
    var get_bus_list = function() {
      send_msg('bus_list', "");
    };

    // return the api
    return {

      messages      : function() {
        return messages
      },

      bus_list      : function() {
        return bus_list
      },

      hikers_list   : function() {
        return hikers_list
      },

      username      : function() {
        return username
      },

      activeChannel : function() {
        return activeChannel
      },

      change_bus : function(bus_name) {
        send_msg('bus_subscribed', bus_name);
      },

      add_bus: function(bus_name) {
        send_msg('add_bus', bus_name);
      },

      set_username : set_username,

      get_hikers: function() {
        send_msg('hitchhicker_list', "");
      },

      send_chat: function(msg) {
        var Packet = {
          sender: username,
          text : msg
        };
        send_msg('chat', Packet);
      }

    }
  })
