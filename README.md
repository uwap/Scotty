# Scotty

Scotty is the [Maschinendeck](http://github.com/Maschinendeck) infoscreen.

## Table of Contents
1. [Client](#client)
  1. [Websocket Data](#1_1)
2. [Server](#server)
  1. [Install / Run] (#2_1)
  
## 1. Client <a name="client"></a>
The client is a plain html, css and javascript webpage having data pushed via a websocket from the [Server](#server).
It uses the [foundation framework](http://foundation.zurb.com).

#### i. Websocket Data <a name="1_1"></a>
The data pushed by [the Scotty Server](#server) is very restricted in information to reduce the traffic to the client.

This is how the room state information looks like:
```{.javascript}
{
  room: {
    state: "open"  
  }
}
```
If the room state from the [space api](http://spaceapi.net/) changes, a json object with `room.state` being one of
`open | closed | unknown` is sent to the client.

If the song played from the MPD changes the json will look like this:
```{.javascript}
{
  mpd: {
    song: "John Cage - 4:33"
  }
}
```
The JSON Object contains an `mpd.song` being a string containing the song name.

## 2. Server <a name="server"></a>
The Scotty Server is written in [Haskell](http://haskell.org) and opens a web socket to the client pushing it changes
of data.

#### i. Install / Run <a name="2_1"></a>
You need to have *ghc* and *cabal* installed on your system.

The simply run
```{.sh}
cabal install --dependencies-only
```
to install the dependencies and then
```{.sh}
cabal run
```
to run the scotty server. If you want to install the server so you can run it globally run
```{.sh}
cabal install
```
and then a scotty executable will be installed to ~/.cabal/bin/. You can go ahead and add it to your $PATH.
