# hoge

Lazarus leaning application. sandbox space.

goal: gdsfeel-lazarus or LazDraw.

## Require
* Free Pascal 3.0.2 or later. 
* Lazurus 1.6.4 or later.


## Changes

latest change at first.

- [Debug][Show Extent bounds]
- [Debug][Show Axis line]
- update menu status [Edit][Clear All]
- enable middle button view move.
- enable wheel zooming.
- support world view port.
- add dependent. [BGRABitmap](https://github.com/bgrabitmap/bgrabitmap) .
- implement [Edit][Clear All]
- show local mouse position on status bar.
- show paint canvas size on status bar.
- points save on quit and load on startup.
- mark circle at click position.

## TODO

* Viewing
	* fit 
	* zooming limit for safety.

* Edit
	* add graphics element selectability.	
* Debug
	* Reveal AppConfigDir. currentry ShowMessage()

* Element Kind
	* Segment

* Data Exchange
	* format variation.
		* csv (current) 
		* json
		* dxf
   * [File][Save As...]. save at any location.
   * AppConfigDir (current) 
  
* Referctering
  * mouse tracking operations.

* Unit Test
