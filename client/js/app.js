$(document).foundation();

$(function() {

  var socket = new WebSocket("ws://localhost:5607/");

  socket.onmessage = function(event) {
    var ev = JSON.parse(event.data);
    if (typeof(ev.room) == typeof({})) roomStatusCallback(ev);
    if (typeof(ev.mpd)  == typeof({})) mpdCallback(ev);
  }

  var updateClock = function() {
    var format = function(x) { return x < 10 ? "0" + x : x; }
    var currentTime = new Date();
    $(".clock .hour").text(format(currentTime.getHours()));
    $(".clock .minute").text(format(currentTime.getMinutes()));
    $(".clock .second").text(format(currentTime.getSeconds()));
  }; updateClock(); setInterval(updateClock, 1000);

  var roomStatusCallback = function(x) {
    var hiddenclass = "hide";
    var root = $(".top-bar .label");
    var open = root.filter(".success");
    var closed = root.filter(".alert");
    var undef = root.filter(".secondary");

    root.each(function() { $(this).addClass(hiddenclass); });

    switch (x.room.state) {
      case "open":
        open.removeClass(hiddenclass);
        break;
      case "closed":
        closed.removeClass(hiddenclass);
        break;
      default:
        undef.removeClass(hiddenclass);
        break;
    }
  }

  var mpdCallback = function(x) {
    $(".mpd").text(x.mpd.song);
  }

});
